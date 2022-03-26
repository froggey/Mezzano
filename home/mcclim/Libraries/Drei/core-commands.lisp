;;; -*- Mode: Lisp; Package: DREI-COMMANDS -*-

;;;  (c) copyright 2004-2005 by
;;;           Robert Strandh (strandh@labri.fr)
;;;  (c) copyright 2004-2005 by
;;;           Elliott Johnson (ejohnson@fasl.info)
;;;  (c) copyright 2005 by
;;;           Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;  (c) copyright 2005 by
;;;           Aleksandar Bakic (a_bakic@yahoo.com)
;;;  (c) copyright 2006 by
;;;           Taylor R. Campbell (campbell@mumble.net)
;;;  (c) copyright 2006 by
;;;           Troels Henriksen (athas@sigkill.dk)

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

;;; Commands that provide access to core DREI features, but are not
;;; strictly necessary.

(in-package :drei-commands)

(define-command (com-overwrite-mode :name t :command-table editing-table) ()
  "Toggle overwrite mode for the current mode.
When overwrite is on, an object entered on the keyboard 
will replace the object after the point. 
When overwrite is off (the default), objects are inserted at point. 
In both cases point is positioned after the new object."
  (setf (overwrite-mode (current-view))
        (not (overwrite-mode (current-view)))))

(set-key 'com-overwrite-mode
	 'editing-table
	 '((:insert)))

(defun set-fill-column (column)
  (if (> column 1)
      (setf (auto-fill-column (current-view)) column)
      (progn (beep) (display-message "Set Fill Column requires an explicit argument."))))

(define-command (com-set-fill-column :name t :command-table fill-table)
    ((column 'integer :prompt "Column Number:"))
  "Set the fill column to the specified value.
You must supply a numeric argument. The fill column is 
the column beyond which automatic line-wrapping will occur. 

The default fill column is 70."
  (set-fill-column column))

(set-key `(com-set-fill-column ,*numeric-argument-marker*)
	 'fill-table
	 '((#\x :control) (#\f)))

(define-command (com-zap-to-object :name t :command-table deletion-table) ()
  "Prompt for an object and kill to the next occurence of that object after point.
Characters can be entered in #\ format."
  (require-minibuffer)
  (let* ((item (handler-case (accept 't :prompt "Zap to Object")
		(error () (progn (beep)
				 (display-message "Not a valid object")
				 (return-from com-zap-to-object nil)))))
	 (item-mark (clone-mark (point)))
	 (current-offset (offset (point))))
    (search-forward item-mark (vector item))
    (delete-range (point) (- (offset item-mark) current-offset))))

(define-command (com-zap-to-character :name t :command-table deletion-table) ()
  "Prompt for a character and kill to the next occurence of that character after point.
FIXME: Accepts a string (that is, zero or more characters) 
terminated by a #\NEWLINE. If a zero length string signals an error. 
If a string of length >1, uses the first character of the string."
  (require-minibuffer)
  (let* ((item-string (handler-case (accept 'string :prompt "Zap to Character") ; Figure out how to get #\d and d.  (or 'string 'character)?
		(error () (progn (beep)
				 (display-message "Not a valid string. ")
				 (return-from com-zap-to-character nil)))))
       (item (subseq item-string 0 1))
       (item-mark (clone-mark (point)))

       (current-offset (offset (point))))
  (if (> (length item-string) 1)
      (display-message "Using just the first character"))
  (search-forward item-mark item)
  (delete-range (point) (- (offset item-mark) current-offset))))

(set-key 'com-zap-to-character
	 'deletion-table
	 '((#\z :meta)))

(define-command (com-open-line :name t :command-table editing-table)
    ((numarg 'integer :prompt "How many lines?" :default 1))
  "Insert a #\Newline and leave point before it.
With a numeric argument greater than 1, insert that many #\Newlines."
  (open-line (point) numarg))

(set-key `(com-open-line ,*numeric-argument-marker*)
	 'editing-table
	 '((#\o :control)))

(defmacro define-mark-unit-command (unit command-table &key
                                    move-point
                                    noun
                                    plural)
  "Define a COM-MARK-<UNIT> for `unit' command and put it in
  `command-table'."
  (labels ((concat (&rest strings)
             (apply #'concatenate 'STRING (mapcar #'string strings)))
           (symbol (&rest strings)
             (intern (apply #'concat strings))))
    (let ((forward (symbol "FORWARD-" unit))
          (backward (symbol "BACKWARD-" unit))
          (noun (or noun (string-downcase unit)))
          (plural (or plural (concat (string-downcase unit) "s"))))
      `(define-command (,(symbol "COM-MARK-" unit)
                         :name t
                         :command-table ,command-table)
           ((count 'integer :prompt ,(concat "Number of " plural) :default 1))
         ,(if (not (null move-point))
              (concat "Place point and mark around the current " noun ".
Put point at the beginning of the current " noun ", and mark at the end. 
With a positive numeric argument, put mark that many " plural " forward. 
With a negative numeric argument, put point at the end of the current 
" noun " and mark that many " plural " backward. 
Successive invocations extend the selection.")
              (concat "Place mark at the next " noun " end.
With a positive numeric argument, place mark at the end of 
that many " plural " forward. With a negative numeric argument, 
place mark at the beginning of that many " plural " backward. 

Successive invocations extend the selection."))
         (unless (eq (command-name *previous-command*) 'com-mark-word)
           (setf (offset (mark)) (offset (point)))
           ,(when (not (null move-point))
                  `(if (plusp count)
                       (,backward (point) (current-syntax))
                       (,forward (point) (current-syntax)))))
         (,forward (mark) (current-syntax) count)))))

(define-mark-unit-command word marking-table)
(define-mark-unit-command expression marking-table)
(define-mark-unit-command paragraph marking-table :move-point t)
(define-mark-unit-command definition marking-table :move-point t)

(set-key `(com-mark-word ,*numeric-argument-marker*)
	 'marking-table
	 '((#\@ :meta)))

(set-key `(com-mark-paragraph ,*numeric-argument-marker*)
	 'marking-table
	 '((#\h :meta)))

(set-key 'com-mark-definition
	 'marking-table
	 '((#\h :control :meta)))

(define-command (com-upcase-region :name t :command-table case-table) ()
  "Convert the region to upper case."
  (upcase-region (mark) (point)))

(define-command (com-downcase-region :name t :command-table case-table) ()
  "Convert the region to lower case."
  (downcase-region (mark) (point)))

(define-command (com-capitalize-region :name t :command-table case-table) ()
  "Capitalize each word in the region."
  (capitalize-region (mark) (point)))

(define-command (com-upcase-word :name t :command-table case-table) ()
  "Convert the characters from point until the next word end to upper case.
Leave point at the word end."
  (upcase-word (point) (current-syntax)))

(set-key 'com-upcase-word
	 'case-table
	 '((#\u :meta)))

(define-command (com-downcase-word :name t :command-table case-table) ()
  "Convert the characters from point until the next word end to lower case.
Leave point at the word end."
  (downcase-word (point) (current-syntax)))

(set-key 'com-downcase-word
	 'case-table
	 '((#\l :meta)))

(define-command (com-capitalize-word :name t :command-table case-table) ()
  "Capitalize the next word.
If point is in a word, convert the next character to 
upper case and the remaining letters in the word to lower case. 
If point is before the start of a word, convert the first character 
of that word to upper case and the rest of the letters to lower case. 

Leave point at the word end."
  (capitalize-word (point) (current-syntax)))

(set-key 'com-capitalize-word
	 'case-table
	 '((#\c :meta)))

(define-command (com-tabify-region :name t :command-table editing-table) ()
  "Replace runs of spaces with tabs in region where possible.
Uses TAB-SPACE-COUNT of the STREAM-DEFAULT-VIEW of the pane."
  (tabify-region (mark) (point)
                 (tab-space-count (current-view))))

(define-command (com-untabify-region :name t :command-table editing-table) ()
  "Replace tabs with equivalent runs of spaces in the region.
Uses TAB-SPACE-COUNT of the STREAM-DEFAULT-VIEW of the pane."
  (untabify-region (mark) (point)
                   (tab-space-count (current-view))))

(define-command (com-set-tab-stops :name t :command-table editing-table)
    ((tab-stops '(sequence (integer 0)) :prompt "List of tab stops"))
  "Accept a list of tab positions (in columns) for the view."
  (setf (drei::tab-stop-columns (current-view)) 
	tab-stops))

(define-command (com-indent-line :name t :command-table indent-table) ()
  (indent-current-line (current-view) (point)))

(set-key 'com-indent-line
	 'indent-table
	 '((#\Tab)))

(set-key 'com-indent-line
	 'indent-table
	 '((#\i :control)))

(define-command (com-newline-and-indent :name t :command-table indent-table) ()
  "Inserts a newline and indents the new line."
  (insert-object (point) #\Newline)
  (indent-current-line (current-view) (point)))

(set-key 'com-newline-and-indent
	 'indent-table
	 '((#\j :control)))

(define-command (com-indent-region :name t :command-table indent-table) ()
  "Indent every line of the current region as specified by the
syntax for the buffer."
  (indent-region (current-view) (point) (mark)))

(define-command (com-delete-indentation :name t :command-table indent-table) ()
  "Join current line to previous non-blank line.
Leaves a single space between the last non-whitespace object 
of the previous line and the first non-whitespace object of 
the current line, and point after that space. If there is no 
previous non-blank line, deletes all whitespace at the 
beginning of the buffer at leaves point there."
  (delete-indentation (current-syntax) (point)))

(set-key 'com-delete-indentation
	 'indent-table
	 '((#\^ :meta)))

(define-command (com-auto-fill-mode :name t :command-table fill-table) ()
  (let ((view (current-view)))
    (setf (auto-fill-mode view)
          (not (auto-fill-mode view)))))

(define-command (com-fill-paragraph :name t :command-table fill-table) ()
  (let ((begin-mark (clone-mark (point)))
        (end-mark (clone-mark (point))))
    (unless (eql (object-before begin-mark) #\Newline)
      (backward-paragraph begin-mark (current-syntax)))
    (unless (eql (object-after end-mark) #\Newline)
      (forward-paragraph end-mark (current-syntax)))
    (do-buffer-region (object offset (current-buffer)
                              (offset begin-mark) (offset end-mark))
      (when (eql object #\Newline)
        (setf object #\Space)))
    (let ((point-backup (clone-mark (point))))
      (setf (offset (point)) (offset end-mark))
      (possibly-fill-line)
      (setf (offset (point)) (offset point-backup)))))

(set-key 'com-fill-paragraph
	 'fill-table
	 '((#\q :meta)))

(define-command (com-beginning-of-buffer :name t :command-table movement-table) ()
  "Move point to the beginning of the buffer."
  (beginning-of-buffer (point)))

(set-key 'com-beginning-of-buffer
	 'movement-table
	 '((#\< :meta)))

(set-key 'com-beginning-of-buffer
	 'movement-table
	 '((:home :control)))

(define-command (com-page-down :name t :command-table view-table) ()
  (page-down (editor-pane (drei-instance)) (current-view)))

(set-key 'com-page-down
	 'view-table
	 '((#\v :control)))

(set-key 'com-page-down
	 'view-table
	 '((:next)))

(define-command (com-page-up :name t :command-table view-table) ()
  (page-up (editor-pane (drei-instance)) (current-view)))

(set-key 'com-page-up
	 'view-table
	 '((#\v :meta)))

(set-key 'com-page-up
	 'view-table
	 '((:prior)))

(define-command (com-end-of-buffer :name t :command-table movement-table) ()
  "Move point to the end of the buffer."
  (end-of-buffer (point)))

(set-key 'com-end-of-buffer
	 'movement-table
	 '((#\> :meta)))

(set-key 'com-end-of-buffer
	 'movement-table
	 '((:end :control)))

(define-command (com-mark-whole-buffer :name t :command-table marking-table) ()
  "Place point at the beginning and mark at the end of the buffer."
  (beginning-of-buffer (point))
  (end-of-buffer (mark)))

(set-key 'com-mark-whole-buffer
	 'marking-table
	 '((#\x :control) (#\h)))

(define-command (com-back-to-indentation :name t :command-table movement-table) ()
  "Move point to the first non-whitespace object on the current line.
If there is no non-whitespace object, leaves point at the end of the line."
  (back-to-indentation (point) (current-syntax)))

(set-key 'com-back-to-indentation
	 'movement-table
	 '((#\m :meta)))

(define-command (com-delete-horizontal-space :name t :command-table deletion-table)
    ((backward-only-p
      'boolean :prompt "Delete backwards only?" :default nil))
  "Delete whitespace around point.
With a numeric argument, only delete whitespace before point."
  (delete-horizontal-space (point) (current-syntax) backward-only-p))

(set-key `(com-delete-horizontal-space ,*numeric-argument-marker*)
	 'deletion-table
	 '((#\\ :meta)))

(define-command (com-just-one-space :name t :command-table deletion-table)
    ((count 'integer :prompt "Number of spaces" :default 1))
  "Delete whitespace around point, leaving a single space.
With a positive numeric argument, leave that many spaces.

FIXME: should distinguish between types of whitespace."
  (just-n-spaces (point) count))

(set-key `(com-just-one-space ,*numeric-argument-marker*)
	 'deletion-table
	 '((#\Space :meta)))

(define-command (com-goto-position :name t :command-table movement-table) 
    ((position 'integer :prompt "Goto Position"))
  "Prompts for an integer, and sets the offset of point to that integer."
  (goto-position (point) position))  

(define-command (com-goto-line :name t :command-table movement-table) 
    ((line-number 'integer :prompt "Goto Line"))
  "Prompts for a line number, and sets point to the beginning of that line.
The first line of the buffer is 1. Giving a number <1 leaves 
point at the beginning of the buffer. Giving a line number 
larger than the number of the last line in the buffer leaves 
point at the beginning of the last line of the buffer."
  (goto-line (point) line-number))

(define-command (com-set-mark :name t :command-table marking-table) ()
  "Set mark to the current position of point."
  (setf (offset (mark)) (offset (point))))

(set-key 'com-set-mark
	 'marking-table
	 '((#\Space :control)))

(define-command (com-exchange-point-and-mark :name t :command-table marking-table) ()
  "Exchange the positions of point and mark."
  (psetf (offset (mark)) (offset (point))
         (offset (point)) (offset (mark))))

(set-key 'com-exchange-point-and-mark
	 'marking-table
	 '((#\x :control) (#\x :control)))

(define-command (com-sort-lines :name t :command-table editing-table)
    ((sort-ascending 'boolean :prompt "Sort in ascending order" :default nil))
  "Sort the lines in the region delimited by current point and
mark. The lines will be lexicographically sorted, ignoring all
non-character objects in the lines. When the command is run, it
will ask whether to sort in ascending or descending order."
  ;; I think the fastest thing is to extract all the lines to an list
  ;; of lines, sort the list, and put the lines back in. The
  ;; cons-memory overhead is probably smaller than writing an in-place
  ;; sort algorithm (though the latter definitely wins on hack value).
  (let ((lines (extract-lines-in-region (point) (mark))))
    (dolist (line (sort lines (if sort-ascending
                                  #'string<=
                                  #'string>=)
                        :key #'(lambda (line)
                                 (coerce (remove-if-not #'character line)
                                         'string))))
      (insert-sequence (point) line)
      (insert-object (point) #\Newline))
    (backward-delete-object (point))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Kill-ring

;; Copies an element from a kill-ring to a buffer at the given offset
(define-command (com-yank :name t :command-table editing-table) ()
  "Insert the objects most recently added to the kill ring at point."
  (alexandria:if-let
      ((string (clime:request-selection *standard-input* :clipboard 'string)))
    (insert-sequence (point) string)
    (handler-case (insert-sequence (point) (kill-ring-yank *kill-ring*))
      (empty-kill-ring ()
        (display-message "Kill ring is empty")))))

(set-key 'com-yank
	 'editing-table
	 '((#\y :control)))

;; Destructively cut a given buffer region into the kill-ring
(define-command (com-kill-region :name t :command-table editing-table) ()
  "Kill the objects between point and mark.
That is, push them onto the kill ring, and delete them from the buffer."
  (kill-region (mark) (point)))

(set-key 'com-kill-region
	 'editing-table
	 '((#\w :control)))

;; Non destructively copies buffer region to the kill ring
(define-command (com-copy-region :name t :command-table marking-table) ()
  "Copy the objects between point and mark to the kill ring."
  (kill-ring-standard-push *kill-ring*
                           (region-to-sequence (point)
                                               (mark))))

(set-key 'com-copy-region
	 'marking-table
	 '((#\w :meta)))

(define-command (com-rotate-yank :name t :command-table editing-table) ()
  "Replace the immediately previously yanked objects with others.
Must be given immediately following a Yank or Rotate Yank command. 
The replacement objects are those before the previously yanked 
objects in the kill ring."
  (handler-case (let ((last-yank (kill-ring-yank *kill-ring*)))
                  (if (member (command-name *previous-command*)
                              '(com-yank com-rotate-yank))
                      (progn
                        (delete-range (point) (* -1 (length last-yank)))
                        (rotate-yank-position *kill-ring*)))
                  (insert-sequence (point)
                                   (kill-ring-yank *kill-ring*)))
    (empty-kill-ring ()
      (display-message "Kill ring is empty"))))

(set-key 'com-rotate-yank
	 'editing-table
	 '((#\y :meta)))

(define-command (com-resize-kill-ring :name t :command-table editing-table) 
    ((size 'integer :prompt "New kill ring size" :default 5))
  "Prompt for a new size for the kill ring.
The default is 5. A number less than 5 will be replaced by 5."
     (setf (kill-ring-max-size *kill-ring*) size))

(define-command (com-append-next-kill :name t :command-table editing-table) ()
  "Set the kill ring to append the next kill to the previous one."
  (setf (append-next-p *kill-ring*) t))

(set-key 'com-append-next-kill
	 'editing-table
	 '((#\w :control :meta)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Undo/redo

(define-command (com-undo :name t :command-table editing-table) ()
  (handler-case (undo (undo-tree (current-buffer)))
    (no-more-undo () (beep) (display-message "No more undo"))))

(set-key 'com-undo
	 'editing-table
	 '((#\_ :control)))

(set-key 'com-undo
	 'editing-table
	 '((#\x :control) (#\u)))

(define-command (com-redo :name t :command-table editing-table) ()
  (handler-case (redo (undo-tree (current-buffer)))
    (no-more-undo () (beep) (display-message "No more redo"))))

(set-key 'com-redo
	 'editing-table
	 '((#\_ :meta)))

(set-key 'com-redo
	 'editing-table
	 '((#\x :control) (#\r :control)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Dynamic abbrevs

(define-command (com-dabbrev-expand :name t :command-table editing-table) ()
  "Expand word before point dynamically.
Search from point (first backward to the beginning of the buffer, 
then forward) for words for which the word before point is a prefix, 
inserting each in turn at point as an expansion."
  (with-accessors ((original-prefix original-prefix)
                   (prefix-start-offset prefix-start-offset)
                   (dabbrev-expansion-mark dabbrev-expansion-mark)) (current-view)
    (flet ((move () (cond ((beginning-of-buffer-p dabbrev-expansion-mark)
                           (setf (offset dabbrev-expansion-mark)
                                 (offset (point)))
                           (forward-word dabbrev-expansion-mark (current-syntax)))
                          ((mark< dabbrev-expansion-mark (point))
                           (backward-object dabbrev-expansion-mark))
                          (t (forward-object dabbrev-expansion-mark)))))
      (unless (or (beginning-of-buffer-p (point))
                  (not (constituentp (object-before (point)))))
        (unless (and (eq (command-name *previous-command*) 'com-dabbrev-expand)
                     (not (null prefix-start-offset)))
          (setf dabbrev-expansion-mark (clone-mark (point)))
          (backward-word dabbrev-expansion-mark (current-syntax))
          (setf prefix-start-offset (offset dabbrev-expansion-mark))
          (setf original-prefix (region-to-sequence prefix-start-offset (point)))
          (move))
        (loop until (or (end-of-buffer-p dabbrev-expansion-mark)
                        (and (or (beginning-of-buffer-p dabbrev-expansion-mark)
                                 (not (constituentp (object-before dabbrev-expansion-mark))))
                             (looking-at dabbrev-expansion-mark original-prefix)))
           do (move))
        (if (end-of-buffer-p dabbrev-expansion-mark)
            (progn (delete-region prefix-start-offset (point))
                   (insert-sequence (point) original-prefix)
                   (setf prefix-start-offset nil))
            (progn (delete-region prefix-start-offset (point))
                   (insert-sequence (point)
                                    (let ((offset (offset dabbrev-expansion-mark)))
                                      (prog2 (forward-word dabbrev-expansion-mark (current-syntax))
                                          (region-to-sequence offset dabbrev-expansion-mark)
                                        (setf (offset dabbrev-expansion-mark) offset))))
                   (move)))))))

(set-key 'com-dabbrev-expand
	 'editing-table
	 '((#\/ :meta)))

(define-command (com-mark-page :name t :command-table marking-table)
    ((count 'integer :prompt "Move how many pages" :default 1)
     (numargp 'boolean :prompt "Move to another page?" :default nil))
  "Place point and mark around the current page.
With a numeric argument, move point that many 
pages forward (backward if negative) before marking the 
surrounding page. When no page delimeters are found, 
leave point at the beginning and mark at the end of the buffer. 

A page is delimited by the sequence #\Newline #\Page."
  (cond ((and numargp (/= 0 count))
         (if (plusp count)
             (forward-page (point) (current-syntax) count)
             (backward-page (point) (current-syntax) (1+ count))))
        (t (backward-page (point) (current-syntax) count)))
  (setf (offset (mark)) (offset (point)))
  (forward-page (mark) (current-syntax) 1))

(set-key `(com-mark-page ,*numeric-argument-marker* ,*numeric-argument-marker*)
	 'marking-table
	 '((#\x :control) (#\p :control)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Commenting

;;; figure out how to make commands without key bindings accept numeric arguments. 
(define-command (com-comment-region :name t :command-table comment-table) ()
  (comment-region (current-syntax) (point) (mark)))

;; (defparameter *insert-pair-alist*
;; 	      '((#\( #\)) (#\[ #\]) (#\{ #\}) (#\< #\>) (#\" #\") (#\' #\') (#\` #\')))

(defun insert-parentheses (mark syntax count)
  (insert-pair mark syntax count #\( #\)))

(define-command (com-insert-parentheses :name t :command-table editing-table)
    ((count 'integer :prompt "Number of expressions" :default 1)
     (wrap-p 'boolean :prompt "Wrap expressions?" :default nil))
  "Insert a pair of parentheses, leaving point in between.
With a numeric argument, enclose that many expressions 
forward (backward if negative)."
  (unless wrap-p (setf count 0))
  (insert-parentheses (point) (current-syntax) count))

(set-key `(com-insert-parentheses ,*numeric-argument-marker* ,*numeric-argument-marker*)
	 'editing-table
	 '((#\( :meta)))

(define-command (com-visible-region :name t :command-table marking-table) ()
  "Toggle the visibility of the region in the current pane."
  (setf (region-visible-p (current-view))
        (not (region-visible-p (current-view)))))

(define-command (com-move-past-close-and-reindent :name t :command-table editing-table)
    ()
  "Move past the next `)' and reindent"
  (move-past-close-and-reindent (current-view) (point)))

(set-key `(com-move-past-close-and-reindent)
	 'editing-table
	 '((#\) :meta)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Rectangle editing

(define-command (com-kill-rectangle :name t :command-table deletion-table)
    ()
  "Kill the rectangle bounded by current point and mark.   

The rectangle will be put in a rectangle kill buffer, from which it can
later be yanked with Yank Rectangle. This kill buffer is completely
disjunct from the standard kill ring and can only hold a single rectangle at a time."
  (setf *killed-rectangle*
        (map-rectangle-lines (current-view)
                             #'extract-and-delete-rectangle-line
                             (point)
                             (mark))))

(set-key 'com-kill-rectangle
         'deletion-table
         '((#\x :control) (#\r) (#\k)))

(define-command (com-delete-rectangle :name t :command-table deletion-table)
    ()
  "Delete the rectangle bounded by current point and mark.

The rectangle will be deleted and NOT put in the kill buffer."
  (map-rectangle-lines (current-view)
                       #'extract-and-delete-rectangle-line
                       (point)
                       (mark)))

(set-key 'com-delete-rectangle
         'deletion-table
         '((#\x :control) (#\r) (#\d)))

(define-command (com-yank-rectangle :name t :command-table editing-table)
    ()
  "Insert the rectangle from the rectangle kill buffer at mark.  

The rectangle kill buffer will not be emptied, so it is possible to yank
the same rectangle several times."
  (insert-rectangle-at-mark (current-view)
                            (point)
                            *killed-rectangle*))

(set-key 'com-yank-rectangle
         'editing-table
         '((#\x :control) (#\r) (#\y)))

(define-command (com-clear-rectangle :name t :command-table deletion-table)
    ()
  "Clear the rectangle bounded by current point and mark by filling it with spaces."
  (map-rectangle-lines (current-view)
                       #'clear-rectangle-line
                       (point)
                       (mark)))

(set-key 'com-clear-rectangle
         'editing-table
         '((#\x :control) (#\r) (#\c)))

(define-command (com-open-rectangle :name t :command-table editing-table)
    ()
  "Open the rectangle bounded by current point and mark.  

The rectangle will not be deleted, but instead pushed to the right, with
the area previously inhabited by it filled with spaces."
  (map-rectangle-lines (current-view)
                       #'open-rectangle-line
                       (point)
                       (mark)))

(set-key 'com-open-rectangle
         'editing-table
         '((#\x :control) (#\r) (#\o)))

(define-command (com-string-rectangle :name t :command-table editing-table)
    ((string 'string :prompt "String rectangle"))
  "Replace each line of the rectangle bounded by current point of mark with `string'.

The length of the string need not be equal to the width of the rectangle."
  (map-rectangle-lines (current-view)
                       #'(lambda (mark startcol endcol)
                           (replace-rectangle-line mark startcol endcol string))
                       (point)
                       (mark)))

(set-key 'com-string-rectangle
         'editing-table
         '((#\x :control) (#\r) (#\t)))

(define-command (com-string-insert-rectangle :name t :command-table editing-table)
    ((string 'string :prompt "String rectangle"))
  "Insert `string' in each line of the rectangle bounded by current point of mark.

Text in the rectangle will be shifted right."
  (map-rectangle-lines (current-view)
                       #'(lambda (mark startcol endcol)
                           (insert-in-rectangle-line mark startcol endcol string))
                       (point)
                       (mark)))

(define-command (com-delete-whitespace-rectangle :name t :command-table editing-table)
    ()
  (map-rectangle-lines (current-view)
                       #'delete-rectangle-line-whitespace
                       (point)
                       (mark)))
