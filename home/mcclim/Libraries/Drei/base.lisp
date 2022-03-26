;;; -*- Mode: Lisp; Package: DREI -*-

;;;  (c) copyright 2004, 2005, 2014 by
;;;           Robert Strandh (robert.strandh@gmail.com)
;;;  (c) copyright 2004-2005 by
;;;           Elliott Johnson (ejohnson@fasl.info)
;;;  (c) copyright 2005 by
;;;           Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;  (c) copyright 2005 by
;;;           Aleksandar Bakic (a_bakic@yahoo.com)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

;;; Basic functionality built on top of the buffer protocol.  Here is
;;; where we define slightly higher level functions such as
;;; {previous,next}-line, {forward,backward}-word, etc. that can be
;;; directly implemented in terms of the buffer protocol, but that are
;;; not, strictly speaking, part of that protocol.

(in-package :drei-base)

(defgeneric invoke-as-region (mark1 mark2 continuation)
  (:documentation "Invoke `continuation' with two arguments
ordering a proper region."))

(defmethod invoke-as-region ((mark1 integer) (mark2 integer)
                             (continuation function))
  (if (>= mark2 mark1)
      (funcall continuation mark1 mark2)
      (funcall continuation mark2 mark1)))

(defmethod invoke-as-region ((mark1 mark) (mark2 mark)
                             (continuation function))
  (if (mark>= mark2 mark1)
      (funcall continuation mark1 mark2)
      (funcall continuation mark2 mark1)))

(defmacro as-region ((mark1 mark2) &body body)
  "Rebind `mark1' and `mark2' to be a proper region. That
is, `(mark>= mark2 mark1)' will hold. `Mark1' and `mark2' must be
symbols bound to marks or integers (but they must be of the same
type). It is a good idea to use this macro when dealing with
regions."
  `(invoke-as-region ,mark1 ,mark2
                     #'(lambda (,mark1 ,mark2)
                         ,@body)))

(defmacro as-full-region ((mark1 mark2) &body body)
  "Bind `mark1' and `mark2' to marks that delimit a full
  region (a region where the beginning and end are at the
  beginning and end of their lines, respectively). The new marks
  will be copies of the marks `mark1' and `mark2' are already
  bound to. `Mark1' and `mark2' must be symbols bound to marks."
  `(as-region (,mark1 ,mark2)
     (let ((,mark1 (clone-mark ,mark1))
           (,mark2 (clone-mark ,mark2)))
       (beginning-of-line ,mark1)
       (end-of-line ,mark2)
       ,@body)))

(defmacro as-offsets ((&rest marks)
                      &body body)
  "Bind the symbols in `marks' to the numeric offsets of the mark
objects that the symbols are bound to. If a symbol in `mark' is
already bound to an offset, just keep that binding. An element of
`marks' may also be a list - in this case, the second element is
used to get an offset, and the first element (which should be a
symbol) will be bound to this offset. Evaluate `body' with these
bindings."
  `(let ,(mapcar #'(lambda (mark-sym)
                     (if (listp mark-sym)
                         `(,(first mark-sym)
                            (let ((value ,(second mark-sym)))
                              (if (numberp value)
                                  value
                                  (offset value))))
                         `(,mark-sym
                           (let ((value ,mark-sym))
                             (if (numberp value)
                                 ,mark-sym
                                 (offset value))))))
                 marks)
     ,@body))

(defmacro do-buffer-region ((object offset buffer offset1 offset2)
                            &body body)
  "Iterate over the elements of the region delimited by offset1 and offset2.
The body is executed for each element, with object being the current object
\(setf-able), and offset being its offset."
  `(symbol-macrolet ((,object (buffer-object ,buffer ,offset)))
     (loop for ,offset from ,offset1 below ,offset2
        do ,@body)))

(defmacro do-buffer-region-lines ((line-var mark1 mark2) &body body)
  "Iterate over the lines in the region delimited by `mark1' and `mark2'.
   For each line, `line-var' will be bound to a mark positioned
   at the beginning of the line and `body' will be executed. Note
   that the iteration will always start from the mark specifying
   the earliest position in the buffer."
  (with-gensyms (mark-sym mark2-sym)
    `(progn
       (let* ((,mark-sym (clone-mark ,mark1))
              (,mark2-sym (clone-mark ,mark2)))
         (as-region (,mark-sym ,mark2-sym)
           (loop while (and (mark<= ,mark-sym ,mark2-sym)
                            (not (end-of-buffer-p ,mark-sym)))
              do
              (let ((,line-var (clone-mark ,mark-sym)))
                ,@body)
              (end-of-line ,mark-sym)
              (unless (end-of-buffer-p ,mark-sym)
                (forward-object ,mark-sym))))))))

(defgeneric previous-line (mark &optional column count)
  (:documentation "Move a mark up `count' lines conserving
  horizontal position. This is a relatively low-level function,
  you should probably use `drei-motion:backward-line'
  instead."))

(defmethod previous-line (mark &optional column (count 1))
  (unless column
    (setf column (column-number mark)))
  (loop repeat count
     do (beginning-of-line mark)
     until (beginning-of-buffer-p mark)
     do (backward-object mark))
  (end-of-line mark)
  (when (> (column-number mark) column)
    (beginning-of-line mark)
    (incf (offset mark) column)))

(defmethod previous-line ((mark p-line-mark-mixin) &optional column (count 1))
  (unless column
    (setf column (column-number mark)))
  (let* ((line (line-number mark))
	 (goto-line (max 0 (- line count))))
    (setf (offset mark)
	  (+ column (buffer-line-offset (buffer mark) goto-line)))))

(defgeneric next-line (mark &optional column count)
  (:documentation "Move a mark down `count' lines conserving
  horizontal position. This is a relatively low-level function,
  you should probably use `drei-motion:forward-line'
  instead."))

(defmethod next-line (mark &optional column (count 1))
  (unless column
    (setf column (column-number mark)))
  (loop repeat count
     do (end-of-line mark)
     until (end-of-buffer-p mark)
     do (forward-object mark))
  (end-of-line mark)
  (when (> (column-number mark) column)
    (beginning-of-line mark)
    (incf (offset mark) column)))

(defmethod next-line ((mark p-line-mark-mixin) &optional column (count 1))
  (unless column
    (setf column (column-number mark)))
  (let* ((line (line-number mark))
         (goto-line (min (number-of-lines (buffer mark))
                         (+ line count))))
    (setf (offset mark)
	  (+ column (buffer-line-offset (buffer mark) goto-line)))))

(defgeneric open-line (mark &optional count)
  (:documentation "Create a new line in a buffer after the mark."))

(defmethod open-line ((mark left-sticky-mark) &optional (count 1))
  (loop repeat count
     do (insert-object mark #\Newline)))

(defmethod open-line ((mark right-sticky-mark) &optional (count 1))
  (loop repeat count
     do (insert-object mark #\Newline)
     (decf (offset mark))))

(defun delete-line (mark &optional (count 1))
  "Delete `count' lines at `mark' from the buffer."
  (dotimes (i count)
    (if (end-of-line-p mark)
        (unless (end-of-buffer-p mark)
          (delete-range mark))
        (let ((offset (offset mark)))
          (end-of-line mark)
          (delete-region offset mark)))))

(defgeneric extract-line (mark &key from-end whole-line)
  (:documentation "Destructively remove part of a line and return
it. The line `mark' is in indicates which line to perform the
extraction on. The line contents from the beginning of the line
up to `mark' will be deleted and returned as a vector. If
`from-end' is true, the line contents from the end of the line to
`mark' will be affected instead. If `whole-line' is true, the
entire line, including any single ending newline character, will
be deleted and returned."))

(defun extract-whole-line (mark)
  "Extract the whole line `mark' is in, and remove any single
  trailing newline."
  (let* ((border-mark (clone-mark mark))
         eol-offset)
    (end-of-line border-mark)
    (setf eol-offset (offset border-mark))
    (unless (end-of-buffer-p border-mark)
      (incf eol-offset))
    (beginning-of-line border-mark)
    (let ((sequence (region-to-sequence border-mark eol-offset)))
      (delete-region border-mark eol-offset)
      sequence)))

(defmethod extract-line ((mark mark) &key from-end whole-line)
  (if whole-line
      (extract-whole-line mark)
      (let ((border-mark (clone-mark mark)))
        (if from-end
            (end-of-line border-mark)
            (beginning-of-line border-mark))
        (as-region (mark border-mark)
          (let ((sequence (region-to-sequence mark border-mark)))
            (delete-region mark border-mark)
            sequence)))))

(defgeneric lines-in-region (mark1 mark2)
  (:documentation "Return a list of all the lines (not including
  newline characters) in the full region delimited by `mark1' and
  `mark2'."))

(defmethod lines-in-region (mark1 mark2)
  (as-full-region (mark1 mark2)
    (let (result)
      (do-buffer-region-lines (line-mark mark1 mark2)
        (let ((bol-offset (offset line-mark)))
          (end-of-line line-mark)
          (push (region-to-sequence bol-offset line-mark) result)))
      result)))

(defgeneric extract-lines-in-region (mark1 mark2)
  (:documentation "Delete the lines of the full region delimited
by `mark1' and `mark2'"))

(defmethod extract-lines-in-region ((mark1 mark) (mark2 mark))
  (as-full-region (mark1 mark2)
    (let ((lines (lines-in-region mark1 mark2)))
      (delete-region mark1 mark2)
      lines)))

(defun empty-line-p (mark)
  "Check whether the mark is in an empty line."
  (and (beginning-of-line-p mark) (end-of-line-p mark)))

(defun line-indentation (mark tab-width)
  "Return the distance from the beginning of the line and the first
constituent character of the line."
  (let ((mark2 (clone-mark mark)))
    (beginning-of-line mark2)
    (if (end-of-line-p mark2)
        0
        (loop with indentation = 0
           as object = (object-after mark2)
           until (end-of-buffer-p mark2)
           while (or (eql object #\Space) (eql object #\Tab))
           do (incf indentation
                    (if (eql (object-after mark2) #\Tab) tab-width 1))
           (incf (offset mark2))
           finally (return indentation)))))

(defgeneric buffer-number-of-lines-in-region (buffer offset1 offset2))

(defmethod buffer-number-of-lines-in-region (buffer offset1 offset2)
  "Helper method for number-of-lines-in-region.  Count newline
characters in the region between offset1 and offset2."
  (loop while (< offset1 offset2)
     count (eql (buffer-object buffer offset1) #\Newline)
     do (incf offset1)))

(defmethod buffer-number-of-lines-in-region
    ((buffer binseq2-buffer) offset1 offset2)
  "Helper method for NUMBER-OF-LINES-IN-REGION."
  (- (buffer-line-number buffer offset2)
     (buffer-line-number buffer offset1)))

(defun buffer-display-column (buffer offset tab-width)
  (let ((line-start-offset (- offset (buffer-column-number buffer offset))))
    (loop with column = 0
       for i from line-start-offset below offset
       do (incf column (if (eql (buffer-object buffer i) #\Tab)
                           (- tab-width (mod column tab-width))
                           1))
       finally (return column))))

(defgeneric number-of-lines-in-region (mark1 mark2)
  (:documentation "Return the number of lines (or rather the
number of Newline characters) in the region between MARK and
MARK2.  An error is signaled if the two marks are positioned in
different buffers.  It is acceptable to pass an offset in place of
one of the marks"))

(defmethod number-of-lines-in-region ((mark1 mark) (mark2 mark))
  (assert (eq (buffer mark1) (buffer mark2)))
  (let ((offset1 (offset mark1))
	(offset2 (offset mark2)))
    (as-region (offset1 offset2)
      (buffer-number-of-lines-in-region (buffer mark1) offset1 offset2))))

(defmethod number-of-lines-in-region ((offset1 integer) (mark2 mark))
  (let ((offset2 (offset mark2)))
    (as-region (offset1 offset2)
      (buffer-number-of-lines-in-region (buffer mark2) offset1 offset2))))

(defmethod number-of-lines-in-region ((mark1 mark) (offset2 integer))
  (let ((offset1 (offset mark1)))
    (as-region (offset1 offset2)
      (buffer-number-of-lines-in-region (buffer mark1) offset1 offset2))))

(defun constituentp (obj)
  "A predicate to ensure that an object is a constituent character."
  (and (characterp obj)
       ;; #+sbcl (sb-impl::constituentp obj)
       (or (alphanumericp obj)
           (member obj '(#\! #\$ #\% #\& #\* #\+ #\- #\. #\/
                         #\: #\< #\= #\> #\? #\@ #\^ #\~ #\_
                         #\{ #\} #\[ #\] #\#)))))

(defun buffer-whitespacep (obj)
  "Return T if `obj' is a basic whitespace character. This
  function does not respect the current syntax."
  (member obj '(#\Space #\Tab #\Newline #\Page #\Return)))

(defun just-n-spaces (mark1 n)
  "Remove all spaces around `mark', leaving behind `n'
spaces. `Mark' will be moved to after any spaces inserted."
  (let ((mark2 (clone-mark mark1)))
    (loop
       while (not (beginning-of-buffer-p mark2))
       while (eql (object-before mark2) #\Space)
       do (backward-object mark2))
    (loop
       while (not (end-of-buffer-p mark1))
       while (eql (object-after mark1) #\Space)
       do (forward-object mark1))
    (let ((existing-spaces (- (offset mark1)
                              (offset mark2))))
      (cond ((= n existing-spaces))
            ((> n existing-spaces)
             (insert-sequence mark1 (make-array (- n existing-spaces)
                                     :initial-element #\Space)))
            ((< n existing-spaces)
             (delete-region (- (offset mark1)
                               (- existing-spaces n))
                            mark1))))))

(defun move-to-column (mark column &optional force)
  "Move the position of `mark' to column number `column'. If the
  line is too short, put `mark' at end of line, unless `force' is
  non-NIL, in which case spaces will be added to the end of the
  line."
  (let ((set-column (setf (column-number mark) column)))
    (when (and (not (= set-column column))
               force)
      (insert-sequence mark (make-string (- column set-column)
                             :initial-element #\Space)))))

(defun kill-region (mark1 mark2)
  "Kill the objects between `mark1' and `mark2', one of which may optionally be an offset.
That is, push the objects of the delimited region onto
`*kill-ring*', and delete them from the buffer."
  (kill-ring-standard-push
   *kill-ring* (region-to-sequence mark1 mark2))
  (delete-region mark1 mark2))

(defun in-place-buffer-substring (buffer string offset1 offset2)
  "Copy from `offset1' to `offset2' in `buffer' to `string',
which must be an adjustable vector of characters with a fill
pointer. All objects in the buffer range must be
characters. Returns `string'."
  (loop for offset from offset1 below offset2
     for i upfrom 0
     do (vector-push-extend (buffer-object buffer offset) string)
     finally (return string)))

(defun fill-string-from-buffer (buffer string offset1 offset2)
  "Copy from `offset1' to `offset2' in `buffer' to `string',
which must be an adjustable vector of characters with a fill
pointer. Once the buffer region has been copied to `string', or a
non-character object has been encountered in the buffer, the
number of characters copied to `string' will be returned."
  (loop for offset from offset1 below offset2
     for i upfrom 0
     if (characterp (buffer-object buffer offset))
     do (vector-push-extend (buffer-object buffer offset) string)
     else do (loop-finish)
     finally (return i)))

(defun buffer-find-nonchar (buffer start-offset max-offset)
  "Search through `buffer' from `start-offset', returning the
first offset at which a non-character object is found, or
`max-offset', whichever comes first."
  (loop for offset from start-offset below max-offset
     unless (characterp (buffer-object buffer offset))
     do (loop-finish)
     finally (return offset)))

(defun offset-beginning-of-line-p (buffer offset)
  "Return true if `offset' is at the beginning of a line in
`buffer' or at the beginning of `buffer'."
  (or (zerop offset) (eql (buffer-object buffer (1- offset)) #\Newline)))

(defun offset-end-of-line-p (buffer offset)
  "Return true if `offset' is at the end of a line in
`buffer' or at the end of `buffer'."
  (or (= (size buffer) offset)
      (eql (buffer-object buffer offset) #\Newline)))

(defun end-of-line-offset (buffer start-offset)
  "Return the offset of the end of the line of `buffer'
containing `start-offset'."
  (loop for offset from start-offset
     until (offset-end-of-line-p buffer offset)
     finally (return offset)))

(defun extract-region (mark-or-offset1 mark-or-offset2)
  "Delete the region delimited by `mark-or-offset1' and
`mark-or-offset2', returning the extracted sequence of objects."
  (prog1 (region-to-sequence mark-or-offset1 mark-or-offset2)
    (delete-region mark-or-offset1 mark-or-offset2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Character case

(defun buffer-region-case (buffer offset1 offset2)
  (let ((possibly-uppercase t)
        (possibly-lowercase t)
        (possibly-capitalized t))
    (do-buffer-region (object offset buffer offset1 offset2)
      (unless (characterp object)
        (return-from buffer-region-case nil))
      (when (lower-case-p object)
        (setf possibly-uppercase nil))
      (when (upper-case-p object)
        (setf possibly-lowercase nil))
      (when (plusp offset)
        (let ((previous-object (buffer-object buffer (1- offset))))
          (when (and (characterp previous-object)
                     (if (constituentp previous-object)
                         (upper-case-p object)
                         (lower-case-p object)))
            (setf possibly-capitalized nil)))))
    (cond (possibly-uppercase :upper-case)
          (possibly-lowercase :lower-case)
          (possibly-capitalized :capitalized)
          (t nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Search

(defun buffer-looking-at (buffer offset vector &key (test #'eql))
  "return true if and only if BUFFER contains VECTOR at OFFSET"
  (and (<= (+ offset (length vector)) (size buffer))
       (loop for i from offset
          for obj across vector
          unless (funcall test (buffer-object buffer i) obj)
          return nil
          finally (return t))))

(defun looking-at (mark vector &key (test #'eql))
  "return true if and only if BUFFER contains VECTOR after MARK"
  (buffer-looking-at (buffer mark) (offset mark) vector :test test))

(defun buffer-search-forward (buffer offset vector &key (test #'eql))
  "return the smallest offset of BUFFER >= OFFSET containing VECTOR
or NIL if no such offset exists"
  (loop for i from offset to (size buffer)
     when (buffer-looking-at buffer i vector :test test)
     return i
     finally (return nil)))

(defun buffer-search-backward (buffer offset vector &key (test #'eql))
  "return the largest offset of BUFFER <= (- OFFSET (length VECTOR))
containing VECTOR or NIL if no such offset exists"
  (loop for i downfrom (- offset (length vector)) to 0
     when (buffer-looking-at buffer i vector :test test)
     return i
     finally (return nil)))

(defun non-greedy-match-forward (a buffer i)
  (let ((p (automaton::initial a)))
    (loop for j from i below (size buffer)
       for q = (automaton::sstep p (buffer-object buffer j)) do
       (unless q
         (return nil))
       (if (automaton::accept q)
           (return (1+ j))
           (setq p q))
       finally (return nil))))

(defun buffer-re-search-forward (a buffer offset)
  "Returns as the first value the smallest offset of BUFFER >= OFFSET
with contents accepted by deterministic automaton A; otherwise,
returns nil. If the first value is non-nil, the second value is the
offset after the matched contents."
  (if (automaton::singleton a)
      (let ((result (buffer-search-forward
		     buffer offset (automaton::singleton a))))
	(when result
	  (values result (+ result (length (automaton::singleton a))))))
      (loop for i from offset below (size buffer) do
           (let ((j (non-greedy-match-forward a buffer i)))
             (when j (return (values i j))))
           finally (return nil))))

(defun reversed-deterministic-automaton (a)
  "Reverses and determinizes A, then returns it."
  (if (automaton::singleton a)
      (progn
	(setf (automaton::singleton a) (reverse (automaton::singleton a)))
	a)
      (automaton::determinize2
       a
       (make-instance 'automaton::state-set :ht (automaton::areverse a)))))

(defun non-greedy-match-backward (a buffer i)
  (let ((p (automaton::initial a)))
    (loop for j downfrom i to 0
       for q = (automaton::sstep p (buffer-object buffer j)) do
       (unless q
         (return nil))
       (if (automaton::accept q)
           (return j)
           (setq p q))
       finally (return nil))))

(defun buffer-re-search-backward (a buffer offset)
  "Returns as the first value the largest offset of BUFFER <= OFFSET
with contents accepted by (reversed) deterministic automaton A;
otherwise, returns nil. If the first value is non-nil, the second
value is the offset after the matched contents."
  (if (automaton::singleton a)
      (let ((result (buffer-search-backward
		     buffer offset (nreverse (automaton::singleton a)))))
	(when result
	  (values result (+ result (length (automaton::singleton a))))))
      (loop for i downfrom (min offset (1- (size buffer))) to 0 do
           (let ((j (non-greedy-match-backward a buffer i)))
             (when j (return (values j (1+ i)))))
           finally (return nil))))

(defun search-forward (mark vector &key (test #'eql))
  "move MARK forward after the first occurence of VECTOR after MARK"
  (let ((offset (buffer-search-forward
		 (buffer mark) (offset mark) vector :test test)))
    (when offset
      (setf (offset mark) (+ offset (length vector))))))

(defun search-backward (mark vector &key (test #'eql))
  "move MARK backward before the first occurence of VECTOR before MARK"
  (let ((offset (buffer-search-backward
		 (buffer mark) (offset mark) vector :test test)))
    (when offset
      (setf (offset mark) offset))))

(defun re-search-forward (mark re)
  "move MARK forward after the first occurence of string matching RE
after MARK"
  (let ((a (automaton::determinize
            (automaton::regexp-automaton
             (automaton::string-regexp re)))))
    (multiple-value-bind (i j)
	(buffer-re-search-forward a (buffer mark) (offset mark))
      (when i
	(setf (offset mark) j)
	(values mark i)))))

(defun re-search-backward (mark re)
  "move MARK backward before the first occurence of string matching RE
before MARK"
  (let ((a (reversed-deterministic-automaton
	    (automaton::regexp-automaton
	     (automaton::string-regexp re)))))
    (multiple-value-bind (i j)
	(buffer-re-search-backward a (buffer mark) (1- (offset mark)))
      (declare (ignorable j))
      (when i
        (setf (offset mark) i)
        (values mark j)))))

(defun buffer-search-word-backward (buffer offset word &key (test #'eql))
  "return the largest offset of BUFFER <= (- OFFSET (length WORD))
containing WORD as a word or NIL if no such offset exists"
  (let ((wlen (length word))
	(blen (size buffer)))
    (loop
       for i downfrom (- offset wlen) to 0
       for j = (+ i wlen)
       when (and (or (zerop i) (buffer-whitespacep (buffer-object buffer (1- i))))
		 (buffer-looking-at buffer i word :test test)
		 (not (and (< (+ i wlen) blen)
			   (constituentp (buffer-object buffer (+ i wlen))))))
       return i
       finally (return nil))))

(defun search-word-backward (mark word)
  (let ((offset (buffer-search-word-backward (buffer mark) (offset mark) word)))
    (when offset
      (setf (offset mark) offset))))

(defun buffer-search-word-forward (buffer offset word &key (test #'eql))
  "Return the smallest offset of BUFFER >= OFFSET containing WORD as a
word or NIL if no such offset exists"
  (let ((wlen (length word))
	(blen (size buffer)))
    (loop
       for i upfrom offset to (- blen (max wlen 1))
       for j = (+ i wlen)
       when (and (or (zerop i) (buffer-whitespacep (buffer-object buffer (1- i))))
		 (buffer-looking-at buffer i word :test test)
		 (not (and (< j blen)
			   (constituentp (buffer-object buffer j)))))
       ;; should this be (+ i wlen)? jqs 2006-05-14
       return i
       finally (return nil))))

(defun search-word-forward (mark word)
  (let ((wlen (length word))
	(offset (buffer-search-word-forward (buffer mark) (offset mark) word)))
    (when offset
      (setf (offset mark) (+ offset wlen)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Character case

;;; I'd rather have update-buffer-range methods spec. on buffer for this,
;;; for performance and history-size reasons --amb
(defun downcase-buffer-region (buffer offset1 offset2)
  (do-buffer-region (object offset buffer offset1 offset2)
    (when (and (constituentp object) (upper-case-p object))
      (setf object (char-downcase object)))))

(defgeneric downcase-region (mark1 mark2)
  (:documentation "Convert all characters after mark1 and before mark2 to
lowercase. An error is signaled if the two marks are positioned in different
buffers. It is acceptable to pass an offset in place of one of the marks."))

(defmethod downcase-region ((mark1 mark) (mark2 mark))
  (assert (eq (buffer mark1) (buffer mark2)))
  (let ((offset1 (offset mark1))
	(offset2 (offset mark2)))
    (as-region (offset1 offset2)
      (downcase-buffer-region (buffer mark1) offset1 offset2))))

(defmethod downcase-region ((offset1 integer) (mark2 mark))
  (let ((offset2 (offset mark2)))
    (as-region (offset1 offset2)
      (downcase-buffer-region (buffer mark2) offset1 offset2))))

(defmethod downcase-region ((mark1 mark) (offset2 integer))
  (let ((offset1 (offset mark1)))
    (as-region (offset1 offset2)
      (downcase-buffer-region (buffer mark1) offset1 offset2))))

(defun upcase-buffer-region (buffer offset1 offset2)
  (do-buffer-region (object offset buffer offset1 offset2)
    (when (and (constituentp object) (lower-case-p object))
      (setf object (char-upcase object)))))

(defgeneric upcase-region (mark1 mark2)
  (:documentation "Convert all characters after mark1 and before mark2 to
uppercase. An error is signaled if the two marks are positioned in different
buffers. It is acceptable to pass an offset in place of one of the marks."))

(defmethod upcase-region ((mark1 mark) (mark2 mark))
  (assert (eq (buffer mark1) (buffer mark2)))
  (let ((offset1 (offset mark1))
	(offset2 (offset mark2)))
    (as-region (offset1 offset2)
      (upcase-buffer-region (buffer mark1) offset1 offset2))))

(defmethod upcase-region ((offset1 integer) (mark2 mark))
  (let ((offset2 (offset mark2)))
    (as-region (offset1 offset2)
      (upcase-buffer-region (buffer mark2) offset1 offset2))))

(defmethod upcase-region ((mark1 mark) (offset2 integer))
  (let ((offset1 (offset mark1)))
    (as-region (offset1 offset2)
      (upcase-buffer-region (buffer mark1) offset1 offset2))))

(defun capitalize-buffer-region (buffer offset1 offset2)
  (let ((previous-char-constituent-p nil))
    (do-buffer-region (object offset buffer offset1 offset2)
      (when (constituentp object)
        (if previous-char-constituent-p
            (when (upper-case-p object)
              (setf object (char-downcase object)))
            (when (lower-case-p object)
              (setf object (char-upcase object)))))
      (setf previous-char-constituent-p (constituentp object)))))

(defgeneric capitalize-region (mark1 mark2)
  (:documentation "Capitalize all words after mark1 and before mark2.
An error is signaled if the two marks are positioned in different buffers.
It is acceptable to pass an offset in place of one of the marks."))

(defmethod capitalize-region ((mark1 mark) (mark2 mark))
  (assert (eq (buffer mark1) (buffer mark2)))
  (let ((offset1 (offset mark1))
	(offset2 (offset mark2)))
    (as-region (offset1 offset2)
      (capitalize-buffer-region (buffer mark1) offset1 offset2))))

(defmethod capitalize-region ((offset1 integer) (mark2 mark))
  (let ((offset2 (offset mark2)))
    (as-region (offset1 offset2)
      (capitalize-buffer-region (buffer mark2) offset1 offset2))))

(defmethod capitalize-region ((mark1 mark) (offset2 integer))
  (let ((offset1 (offset mark1)))
    (as-region (offset1 offset2)
      (capitalize-buffer-region (buffer mark1) offset1 offset2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Tabify

(defun tabify-buffer-region (buffer offset1 offset2 tab-width)
  (flet ((looking-at-spaces (buffer offset count)
           (loop for i from offset
              repeat count
              unless (char= (buffer-object buffer i) #\Space)
              return nil
              finally (return t))))
    (loop for offset = offset1 then (1+ offset)
       until (>= offset offset2)
       do (let* ((column (buffer-display-column
                          buffer offset tab-width))
                 (count (- tab-width (mod column tab-width))))
            (when (looking-at-spaces buffer offset count)
              (finish-output)
              (delete-buffer-range buffer offset count)
              (insert-buffer-object buffer offset #\Tab)
              (decf offset2 (1- count)))))))

(defgeneric tabify-region (mark1 mark2 tab-width)
  (:documentation "Replace sequences of tab-width spaces with tabs
in the region delimited by mark1 and mark2."))

(defmethod tabify-region ((mark1 mark) (mark2 mark) tab-width)
  (assert (eq (buffer mark1) (buffer mark2)))
  (let ((offset1 (offset mark1))
	(offset2 (offset mark2)))
    (as-region (offset1 offset2)
      (tabify-buffer-region (buffer mark1) offset1 offset2 tab-width))))

(defmethod tabify-region ((offset1 integer) (mark2 mark) tab-width)
  (let ((offset2 (offset mark2)))
    (as-region (offset1 offset2)
      (tabify-buffer-region (buffer mark2) offset1 offset2 tab-width))))

(defmethod tabify-region ((mark1 mark) (offset2 integer) tab-width)
  (let ((offset1 (offset mark1)))
    (as-region (offset1 offset2)
      (tabify-buffer-region (buffer mark1) offset1 offset2 tab-width))))

(defun untabify-buffer-region (buffer offset1 offset2 tab-width)
  (loop for offset = offset1 then (1+ offset)
     until (>= offset offset2)
     when (char= (buffer-object buffer offset) #\Tab)
     do (let* ((column (buffer-display-column buffer
                                              offset
                                              tab-width))
               (count (- tab-width (mod column tab-width))))
          (delete-buffer-range buffer offset 1)
          (loop repeat count
             do (insert-buffer-object buffer offset #\Space))
          (incf offset (1- count))
          (incf offset2 (1- count)))))

(defgeneric untabify-region (mark1 mark2 tab-width)
  (:documentation "Replace tabs with tab-width spaces in the region
delimited by mark1 and mark2."))

(defmethod untabify-region ((mark1 mark) (mark2 mark) tab-width)
  (assert (eq (buffer mark1) (buffer mark2)))
  (let ((offset1 (offset mark1))
	(offset2 (offset mark2)))
    (as-region (offset1 offset2)
      (untabify-buffer-region (buffer mark1) offset1 offset2 tab-width))))

(defmethod untabify-region ((offset1 integer) (mark2 mark) tab-width)
  (let ((offset2 (offset mark2)))
    (as-region (offset1 offset2)
      (untabify-buffer-region (buffer mark2) offset1 offset2 tab-width))))

(defmethod untabify-region ((mark1 mark) (offset2 integer) tab-width)
  (let ((offset1 (offset mark1)))
    (as-region (offset1 offset2)
      (untabify-buffer-region (buffer mark1) offset1 offset2 tab-width))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Narrowed marks

(defclass narrowed-mark-mixin ()
  ((low-border-mark :reader low-border-mark
                    :initarg :low-mark
                    :initform (error "A low border mark must be provided"))
   (high-border-mark :reader high-border-mark
                     :initarg :high-mark
                     :initform (error "A high border mark must be provided"))))

(defmethod shared-initialize :after ((obj narrowed-mark-mixin) slot-names &key)
  (with-slots (low-border-mark high-border-mark) obj
    (check-type low-border-mark left-sticky-mark)
    (check-type high-border-mark right-sticky-mark)))

(defgeneric make-narrowed-mark (mark &key low-mark high-mark)
  (:method :before (mark &key (low-mark mark) (high-mark mark))
           (assert (mark>= mark low-mark))
           (assert (mark>= high-mark mark))))

;;; Destructively narrowing marks (the fun part!)...

(defgeneric narrow-mark (mark low-mark high-mark)
  (:documentation "Change `mark' to be a narrowed mark. HIGHLY
IMPORTANT WARNING: This is done **destructively** by modifying
which class `mark' is an instance of."))

(defgeneric unnarrow-mark (mark)
  (:documentation "Change `mark' to be a standard, unnarrowed
mark. HIGHLY IMPORTANT WARNING: This is done **destructively** by
modifying which class `mark' is an instance of."))

;;; Make some common narrowed mark operations easier to do.

(defun make-backward-narrowed-mark (mark &optional (backward-mark mark))
  (make-narrowed-mark mark
   :low-mark backward-mark
   :high-mark (let ((m (clone-mark mark :right)))
                (end-of-buffer m)
                m)))

(defun make-forward-narrowed-mark (mark &optional (forward-mark mark))
  (make-narrowed-mark mark
   :low-mark (let ((m (clone-mark mark :left)))
               (beginning-of-buffer m)
               m)
   :high-mark forward-mark))

(defclass narrowed-standard-left-sticky-mark (narrowed-mark-mixin
                                              standard-left-sticky-mark)
  ())

(defclass narrowed-standard-right-sticky-mark (narrowed-mark-mixin
                                               standard-right-sticky-mark)
  ())

(defmethod make-narrowed-mark ((mark standard-left-sticky-mark)
                               &key (low-mark mark) (high-mark mark))
  (make-instance 'narrowed-standard-left-sticky-mark
   :buffer (buffer mark) :offset (offset mark)
   :low-mark low-mark :high-mark high-mark))

(defmethod make-narrowed-mark ((mark standard-right-sticky-mark)
                               &key (low-mark mark) (high-mark mark))
  (make-instance 'narrowed-standard-right-sticky-mark
   :buffer (buffer mark) :offset (offset mark)
   :low-mark low-mark :high-mark high-mark))

(defmethod clone-mark ((mark narrowed-standard-left-sticky-mark) &optional stick-to)
  (cond ((or (null stick-to) (eq stick-to :left))
	 (make-instance 'narrowed-standard-left-sticky-mark
          :buffer (buffer mark) :offset (offset mark)
          :low-mark (low-border-mark mark)
          :high-mark (high-border-mark mark)))
	((eq stick-to :right)
	 (make-instance 'narrowed-standard-right-sticky-mark
          :buffer (buffer mark) :offset (offset mark)
          :low-mark (low-border-mark mark)
          :high-mark (high-border-mark mark)))
	(t (error "invalid value for stick-to"))))

(defmethod clone-mark ((mark narrowed-standard-right-sticky-mark) &optional stick-to)
  (cond ((or (null stick-to) (eq stick-to :right))
	 (make-instance 'narrowed-standard-right-sticky-mark
          :buffer (buffer mark) :offset (offset mark)
          :low-mark (low-border-mark mark)
          :high-mark (high-border-mark mark)))
	((eq stick-to :left)
	 (make-instance 'narrowed-standard-left-sticky-mark
          :buffer (buffer mark) :offset (offset mark)
          :low-mark (low-border-mark mark)
          :high-mark (high-border-mark mark)))
	(t (error "invalid value for stick-to"))))

(defmethod (setf offset) :before (new-offset (mark narrowed-mark-mixin))
  (assert (<= (offset (low-border-mark mark)) new-offset) ()
	  (make-condition 'motion-before-beginning :offset new-offset))
  (assert (<= new-offset (offset (high-border-mark mark))) ()
	  (make-condition 'motion-after-end :offset new-offset)))

(defmethod beginning-of-buffer-p ((mark narrowed-mark-mixin))
  (mark= mark (low-border-mark mark)))

(defmethod end-of-buffer-p ((mark narrowed-mark-mixin))
  (mark= mark (high-border-mark mark)))

(defmethod narrow-mark ((mark standard-left-sticky-mark)
                        (low-mark left-sticky-mark)
                        (high-mark right-sticky-mark))
  (assert (and (mark<= low-mark mark)
               (mark<= mark high-mark)))
  (change-class mark 'narrowed-standard-left-sticky-mark
   :low-mark low-mark
   :high-mark high-mark))

(defmethod narrow-mark ((mark standard-right-sticky-mark)
                        (low-mark left-sticky-mark)
                        (high-mark right-sticky-mark))
  (assert (and (mark<= low-mark mark)
               (mark<= mark high-mark)))
  (change-class mark 'narrowed-standard-right-sticky-mark
   :low-mark low-mark
   :high-mark high-mark))

(defmethod unnarrow-mark ((mark narrowed-standard-left-sticky-mark))
  (change-class mark 'standard-left-sticky-mark))

(defmethod unnarrow-mark ((mark narrowed-standard-right-sticky-mark))
  (change-class mark 'standard-right-sticky-mark))

(defclass narrowed-delegating-left-sticky-mark (narrowed-mark-mixin
                                                delegating-left-sticky-mark)
  ())

(defclass narrowed-delegating-right-sticky-mark (narrowed-mark-mixin
                                                 delegating-right-sticky-mark)
  ())

(defmethod make-narrowed-mark ((mark delegating-left-sticky-mark)
                               &key (low-mark mark) (high-mark mark))
  (make-instance 'narrowed-delegating-left-sticky-mark
   :buffer (buffer mark) :offset (offset mark)
   :low-mark low-mark :high-mark high-mark))

(defmethod make-narrowed-mark ((mark delegating-right-sticky-mark)
                               &key (low-mark mark) (high-mark mark))
  (make-instance 'narrowed-delegating-right-sticky-mark
   :buffer (buffer mark) :offset (offset mark)
   :low-mark low-mark :high-mark high-mark))

(defmethod clone-mark ((mark narrowed-delegating-left-sticky-mark) &optional stick-to)
  (cond ((or (null stick-to) (eq stick-to :left))
	 (make-instance 'narrowed-delegating-left-sticky-mark
          :implementation (clone-mark (implementation mark) :left)
          :buffer (buffer mark) :low-mark (low-border-mark mark)
          :high-mark (high-border-mark mark)))
	((eq stick-to :right)
	 (make-instance 'narrowed-delegating-right-sticky-mark
          :implementation (clone-mark (implementation mark) :right)
          :buffer (buffer mark) :low-mark (low-border-mark mark)
          :high-mark (high-border-mark mark)))
	(t (error "invalid value for stick-to"))))

(defmethod clone-mark ((mark narrowed-delegating-right-sticky-mark) &optional stick-to)
  (cond ((or (null stick-to) (eq stick-to :right))
         (make-instance 'narrowed-delegating-right-sticky-mark
          :implementation (clone-mark (implementation mark) :right)
          :buffer (buffer mark) :low-mark (low-border-mark mark)
          :high-mark (high-border-mark mark)))
	((eq stick-to :left)
	 (make-instance 'narrowed-delegating-left-sticky-mark
          :implementation (clone-mark (implementation mark) :left)
          :buffer (buffer mark) :low-mark (low-border-mark mark)
          :high-mark (high-border-mark mark)))
	(t (error "invalid value for stick-to"))))

(defmethod narrow-mark ((mark delegating-left-sticky-mark)
                        (low-mark left-sticky-mark)
                        (high-mark right-sticky-mark))
  (assert (and (mark<= low-mark mark)
               (mark<= mark high-mark)))
  (change-class mark 'narrowed-delegating-left-sticky-mark
   :low-mark low-mark
   :high-mark high-mark))

(defmethod narrow-mark ((mark delegating-right-sticky-mark)
                        (low-mark left-sticky-mark)
                        (high-mark right-sticky-mark))
  (assert (and (mark<= low-mark mark)
               (mark<= mark high-mark)))
  (change-class mark 'narrowed-delegating-right-sticky-mark
   :low-mark low-mark
   :high-mark high-mark))

(defmethod unnarrow-mark ((mark narrowed-delegating-left-sticky-mark))
  (change-class mark 'delegating-left-sticky-mark))

(defmethod unnarrow-mark ((mark narrowed-delegating-right-sticky-mark))
  (change-class mark 'delegating-right-sticky-mark))
