;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000 by Robert Strandh (strandh@labri.u-bordeaux.fr)
;;;  (c) copyright 2002 by Tim Moore (moore@bricoworks.com)

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

(in-package :clim-internals)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Command tables

(defgeneric command-table-name (command-table))
(defgeneric command-table-inherit-from (command-table))

;;; Container for info about a command

(defclass command-item ()
  ((command-name :initarg :command-name :reader command-item-name
		      :initarg nil)
   (command-line-name :initarg :command-line-name :reader command-line-name)))

(defmethod print-object ((obj command-item) stream)
  (print-unreadable-object (obj stream :identity t :type t)
    (cond ((slot-boundp obj 'command-line-name)
	   (format stream "~A" (command-line-name obj)))
	  ((slot-boundp obj 'command-name)
	   (format stream "~S" (command-item-name obj)))
	  (t nil))))

;;; According to the specification, command menu items are stored as
;;; lists.  This way seems better, and I hope nothing will break.
(defclass %menu-item (command-item)
  ((menu-name :reader command-menu-item-name :initarg :menu-name)
   (type :initarg :type :reader command-menu-item-type)
   (value :initarg :value :reader command-menu-item-value)
   (documentation :initarg :documentation)
   (text-style :initarg :text-style :reader command-menu-item-text-style :initform nil)
   (keystroke :initarg :keystroke)))

(defmethod print-object ((item %menu-item) stream)
  (print-unreadable-object (item stream :identity t :type t)
    (when (slot-boundp item 'menu-name)
      (format stream "~S" (command-menu-item-name item)))
    (when (slot-boundp item 'keystroke)
      (format stream "~:[~; ~]keystroke ~A"
	      (slot-boundp item 'menu-name)
	      (slot-value item 'keystroke)))))

(defun command-menu-item-options (menu-item)
  (with-slots (documentation text-style) menu-item
    (list :documentation documentation :text-style text-style)))

(defclass standard-command-table (command-table)
  ((name :initarg :name :reader command-table-name)
   (inherit-from :initarg :inherit-from
		 :initform '()
		 :reader command-table-inherit-from
                 :type list)
   (commands  :accessor commands :initarg :commands
	     :initform (make-hash-table :test #'eq))
   (command-line-names :accessor command-line-names
		       :initform (make-hash-table :test #'equal))
   (presentation-translators :reader presentation-translators
			     :initform (make-instance 'translator-table))
   (inherit-menu :reader inherit-menu
                 :initform nil
                 ;; We interpret :menu to mean "inherit menu items
                 ;; without keystrokes" and :keystrokes to mean
                 ;; "inherit menu items with keystrokes".
                 :type (member nil t :menu :keystrokes)
                 :initarg :inherit-menu)
   (menu :initarg :menu :initform '())
   (keystroke-accelerators :initform nil)
   (keystroke-items :initform nil)))

(defmethod print-object ((table standard-command-table) stream)
  (print-unreadable-object (table stream :identity t :type t)
    (format stream "~S" (command-table-name table))))

;;; We store command-table designators, but this function should
;;; return command table objects.
(defmethod command-table-inherit-from :around
    ((command-table standard-command-table))
  (mapcar #'find-command-table (call-next-method)))

;;; Franz user manual says that this slot is setf-able
(defgeneric (setf command-table-inherit-from) (inherit-from table))

(defmethod (setf command-table-inherit-from)
    (inherit (table standard-command-table))
  (invalidate-translator-caches)
  (setf (slot-value table 'inherit-from) inherit))

(defun inherit-keystrokes (command-table)
  "Return true if `command-table' (which must be a command table
designator) inherits keystrokes."
  (let ((inherit-menu (inherit-menu (find-command-table command-table))))
    (or (eq inherit-menu t)
        (eq inherit-menu :keystrokes))))

(defun inherit-menu-items (command-table)
  "Return true if `command-table' (which must be a command table
designator) inherits menu items."
  (let ((inherit-menu (inherit-menu (find-command-table command-table))))
    (or (inherit-keystrokes command-table)
        (eq inherit-menu :menu))))

(defparameter *command-tables* (make-hash-table :test #'eq))

(defparameter *command-parser-table* (make-hash-table)
  "Mapping from command names to argument parsing functions.")

(defvar *unsupplied-argument-marker* '%unsupplied-argument-marker%)
(defvar *numeric-argument-marker* '%numeric-argument-marker%)

(define-condition command-table-error (simple-error)
  ((command-table-name :reader error-command-table-name
                       :initform nil
                       :initarg :command-table-name))
  (:default-initargs :format-control "" :format-arguments nil))

(defmethod print-object ((object command-table-error) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (when (error-command-table-name object)
      (princ (error-command-table-name object) stream))))

(defun command-table-designator-as-name (designator)
  "Return the name of `designator' if it is a command table,
`designator' otherwise."
  (if (typep designator 'standard-command-table)
      (command-table-name designator)
      designator))

(define-condition command-table-not-found (command-table-error)
  ())

(define-condition command-table-already-exists (command-table-error)
  ())

(define-condition command-not-present (command-table-error)
  ())

(define-condition command-not-accessible (command-table-error)
  ())

(define-condition command-already-present (command-table-error)
  ())

(defun find-command-table (name &key (errorp t))
  (cond ((command-table-p name) name)
	((gethash name *command-tables*))
	(errorp (error 'command-table-not-found :command-table-name name))
	(t nil)))

(define-presentation-method present (object (type command-table) stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore for-context-type))
  (let ((name (command-table-name object)))
    (if acceptably
	(prin1 name stream)
	(princ name stream))))


(define-presentation-method accept ((type command-table) stream
				    (view textual-view)
				    &key)
  (multiple-value-bind (table success string)
      (completing-from-suggestions (stream)
	(loop
	   for name being the hash-key of *command-tables*
	     using (hash-value table)
	   do (suggest (symbol-name name) table)))
    (if success
	table
	(simple-parse-error "~A is not the name of a command table" string))))

(defun menu-items-from-list (menu)
  (mapcar
   #'(lambda (item)
       (destructuring-bind (name type value
                                 &rest args)
           item
         (apply #'make-menu-item name type value
                args)))
   menu))

(setf (gethash 'global-command-table *command-tables*)
      (make-instance 'standard-command-table
		     :name 'global-command-table
		     :inherit-from nil
		     :menu nil))

; adjusted to allow anonymous command-tables for menu-bars
(defun make-command-table (name &key inherit-from menu inherit-menu (errorp t))
  (unless inherit-from
    (setq inherit-from '(global-command-table)))
  (if (and name errorp (gethash name *command-tables*))
      (error 'command-table-already-exists :command-table-name name)
      (let ((result (make-instance 'standard-command-table :name name
	                 :inherit-from inherit-from
                         :inherit-menu inherit-menu
	                 :menu (menu-items-from-list menu))))
        (when name
          (setf (gethash name *command-tables*) result))
        result)))

(make-command-table 'user-command-table)

(defmacro define-command-table (name &key (inherit-from '(global-command-table))
                                          (menu nil menu-supplied-p)
                                          inherit-menu)
  `(if-let ((old-table (gethash ',name *command-tables* nil)))
     (with-slots (inherit-from menu) old-table
       (setq inherit-from ',inherit-from)
       ,(when menu-supplied-p
          `(setq menu (menu-items-from-list ',menu)))
       old-table)
     (make-command-table ',name
			 :inherit-from ',inherit-from
                         :inherit-menu ,inherit-menu
			 :menu ',menu
			 :errorp nil)))

(defun remove-command-from-command-table (command-name
					  command-table
					  &key (errorp t))
  (let* ((table (find-command-table command-table))
	 (item (gethash command-name (commands table))))
    (if (null item)
	(when errorp
	  (error 'command-not-present :command-table-name (command-table-name command-table)))
	(progn
	  (when (typep item '%menu-item)
	    (remove-menu-item-from-command-table table
						 (command-menu-item-name item)
                                                 :errorp nil))

          (when (command-item-name item)
            (remhash (command-item-name item) (command-line-names table)))
          (remhash command-name (commands table))))))

(defun add-command-to-command-table (command-name
				     command-table
				     &key name menu keystroke (errorp t)
				     (menu-command (and menu
							`(,command-name))))

  (let ((table (find-command-table command-table))
	(name (cond ((stringp name)
		     name)
		    (name
		     (command-name-from-symbol command-name))
		    (t nil))))
    (multiple-value-bind (menu-name menu-options)
	(cond ((null menu)
	       nil)
	      ((stringp menu)
	       menu)
	      ((eq menu t)
	       (if (stringp name)
		   name
		   (command-name-from-symbol command-name)))
	      ((consp menu)
	       (values (car menu) (cdr menu))))
      (when keystroke
        (add-keystroke-to-command-table table keystroke
					:command command-name :errorp nil))
      (let* ((item (if menu
		       (apply #'make-menu-item
			      menu-name :command menu-command
			      :command-name command-name
			      :command-line-name name
			      `(,@(and keystroke `(:keystroke ,keystroke))
				,@menu-options))
		       (make-instance 'command-item
				      :command-name command-name
				      :command-line-name name)))
	     (after (getf menu-options :after)))
	(when (and errorp (gethash command-name (commands table)))
	  (error 'command-already-present :command-table-name command-table))
	(remove-command-from-command-table command-name table :errorp nil)
	(setf (gethash command-name (commands table)) item)
	(when name
	  (setf (gethash name (command-line-names table)) command-name))
	(when menu
	  (%add-menu-item table item after))))))


(defun apply-with-command-table-inheritance (fun command-table)
  (funcall fun command-table)
  (mapc #'(lambda (inherited-command-table)
	    (apply-with-command-table-inheritance
	     fun (find-command-table inherited-command-table)))
	(command-table-inherit-from command-table)))

 ;;; do-command-table-inheritance has been shipped off to utils.lisp.

(defun map-over-command-table-commands (function command-table
					&key (inherited t))
  (let ((command-table (find-command-table command-table)))
    (flet ((map-func (table)
	     (maphash #'(lambda (key val)
			  (declare (ignore val))
			  (funcall function key))
		      (slot-value table 'commands))))
      (if inherited
	  (apply-with-command-table-inheritance #'map-func command-table)
	  (map-func command-table)))))

(defun map-over-command-table-names (function command-table &key (inherited t))
  (let ((command-table (find-command-table command-table)))
    (flet ((map-func (table)
	     (maphash function (slot-value table 'command-line-names))))
      (if inherited
	  (apply-with-command-table-inheritance #'map-func command-table)
	  (map-func command-table)))))

(defun command-present-in-command-table-p (command-name command-table)
  (let ((table (find-command-table command-table)))
    (if (gethash command-name (slot-value table 'commands))
	table
	nil)))

(defun command-accessible-in-command-table-p (command-name command-table)
  (or (command-present-in-command-table-p command-name command-table)
      (some #'(lambda (table)
		(command-accessible-in-command-table-p
		 command-name
		 (find-command-table table)))
	    (command-table-inherit-from (find-command-table command-table)))))

(defun find-command-from-command-line-name (name command-table &key (errorp t))
  (apply-with-command-table-inheritance
   #'(lambda (table)
       (let ((value (gethash name (command-line-names table))))
	 (when value
	   (return-from find-command-from-command-line-name
	     (values value table)))))
   (find-command-table command-table))
  (if errorp
      (error 'command-not-accessible :command-table-name command-table)))

(defun command-line-name-for-command (command-name command-table
				      &key (errorp t))
  (do-command-table-inheritance (table command-table)
    (let* ((command-item (gethash command-name (slot-value table 'commands)))
	   (command-line-name (and command-item
				   (command-line-name command-item))))
      (when (stringp command-line-name)
	(return-from command-line-name-for-command command-line-name))))
  (cond ((eq errorp :create)
	 (command-name-from-symbol command-name))
	(errorp
	 (error 'command-not-accessible :command-table-name
                (command-table-designator-as-name command-table)))
	(t nil)))

(defun find-menu-item (menu-name command-table &key (errorp t))
  (let* ((table (find-command-table command-table))
	 (mem (member menu-name (slot-value table 'menu)
               :key #'command-menu-item-name :test #'string-equal)))
    (if mem
        (values (car mem) command-table)
        (or (find-if #'(lambda (table)
                         (find-menu-item menu-name table :errorp nil))
                     (command-table-inherit-from table))
            (when errorp
              (error 'command-not-accessible :command-table-name
                     (command-table-designator-as-name table)))))))

(defun remove-menu-item-from-command-table (command-table string
					    &key (errorp t))
  "Removes item from the `command-table'."
  (let ((table (find-command-table command-table))
	(item (find-menu-item string command-table :errorp nil)))
    (with-slots (menu) table
      (if (and errorp (not item))
	  (error 'command-not-present :command-table-name
                 (command-table-designator-as-name table))
	  (setf menu (delete string menu
			     :key #'command-menu-item-name
			     :test #'string-equal))))))

(defun make-menu-item (name type value
		       &key (documentation nil documentationp)
		       (keystroke nil keystrokep)
		       (text-style nil text-style-p)
		       (command-name nil command-name-p)
		       (command-line-name nil command-line-name-p)
		       &allow-other-keys)
  ;; v-- this may be wrong, we do this to allow
  ;; text-style to contain make-text-style calls
  ;; so we use a very limited evaluator - FIXME
  (when (and (consp text-style)
	   (eq (first text-style) 'make-text-style))
    (setq text-style (apply #'make-text-style (rest text-style))))
  (apply #'make-instance '%menu-item
	 :menu-name name :type type :value value
	 `(,@(and documentationp `(:documentation ,documentation))
	   ,@(and keystrokep `(:keystroke ,keystroke))
	   ,@(and text-style-p `(:text-style ,text-style))
	   ,@(and command-name-p `(:command-name ,command-name))
	   ,@(and command-line-name-p
		  `(:command-line-name ,command-line-name)))))

(defun %add-menu-item (command-table item after)
  (with-slots (menu)
      command-table
    (when (null menu)
      (setf after :start))
    (case after
      (:start (push item menu))
      ((:end nil) (setf menu (nconc menu (list item))))
      (:sort (setf menu (sort (cons item menu)
                              #'string-lessp
                              :key #'command-menu-item-name)))
      (t (push item
               (cdr (member after menu
                     :key #'command-menu-item-name
                     :test #'string-equal))))))
  (when (and (slot-boundp item 'keystroke)
             (slot-value item 'keystroke))
    (%add-keystroke-item command-table (slot-value item 'keystroke) item nil)))


(defun add-menu-item-to-command-table (command-table
				       string type value
				       &rest args
				       &key documentation (after :end)
                                         keystroke text-style (errorp t))
  "Adds menu item to the command table."
  (declare (ignore documentation keystroke text-style))
  (let* ((table (find-command-table command-table))
	 (old-item (find-menu-item string command-table :errorp nil)))
    (cond ((and errorp old-item)
	   (error 'command-already-present :command-table-name
                  (command-table-designator-as-name table)))
	  (old-item
	   (remove-menu-item-from-command-table command-table string))
	  (t nil))
    (%add-menu-item table
		    (apply #'make-menu-item string type value args)
		    after)))

(defun map-over-command-table-menu-items (function command-table)
  "Applies function to all of the items in `command-table's
menu. `Command-table' must be a command table or the name of a
command table. `Function' must be a function of three arguments,
the menu name, the keystroke accelerator gesture (which will be
NIL if there is none), and the command menu item; it has dynamic
extent. The command menu items are mapped over in the order
specified by `add-menu-item-to-command-table'. `Command-table' is
a command table designator. Any inherited menu items will be
mapped over after `command-table's own menu items.

`Map-over-command-table-menu-items' does not descend into
sub-menus. If the programmer requires this behavior, he should
examine the type of the command menu item to see if it is
`:menu'."
  (let ((table-object (find-command-table command-table)))
    (flet ((map-table-entries (table)
             (mapc #'(lambda (item)
                       (with-slots (menu-name keystroke) item
                         (funcall function
                                  menu-name
                                  (and (slot-boundp item 'keystroke) keystroke)
                                  item)))
                   (slot-value table 'menu))))
      (map-table-entries table-object)
      (when (inherit-menu-items table-object)
        (dolist (table (command-table-inherit-from table-object))
          (map-over-command-table-menu-items function table))))
    (values)))

(defun map-over-command-table-translators
    (function command-table &key (inherited t))
  (flet ((map-func (table)
           (alexandria:maphash-values
            (lambda (translator)
              (funcall function translator))
            (slot-value (presentation-translators table) 'translators))))
    (let ((command-table (find-command-table command-table)))
      (if inherited
          (apply-with-command-table-inheritance #'map-func command-table)
          (map-func command-table)))))

(defun find-presentation-translator
    (translator-name command-table &key (errorp t))
  (let* ((table (find-command-table command-table))
         (translators (presentation-translators table))
         (translator (gethash translator-name
                              (slot-value translators 'translators))))
    (when (and errorp (null translator))
      (error 'command-not-present :command-table-name command-table))
    translator))

(defun add-presentation-translator-to-command-table
    (command-table translator &key (errorp t))
  (let ((translators (presentation-translators
                      (find-command-table command-table))))
    (when (and errorp
               (nth-value
                1 (gethash (name translator)
                           (slot-value translators 'translators))))
      (error 'command-already-present :command-table-name command-table))
    (add-translator translators translator)))

(defun remove-presentation-translator-from-command-table
    (command-table translator-name &key (errorp t))
  (let* ((translators (presentation-translators
                       (find-command-table command-table)))
         (translator (gethash translator-name
                              (slot-value translators 'translators))))

    (cond ((not (null translator))
           (remove-translator translators translator))
          (errorp
           (error 'command-not-present :command-table-name command-table)))))

;; At this point we should still see the gesture name as supplied by the
;; programmer in 'gesture'
(defun %add-keystroke-item (command-table gesture item errorp)
  (with-slots (keystroke-accelerators keystroke-items)
      command-table
    (let* ((gesture (if (and (symbolp gesture) ; symbolic gesture name?
                             (gethash gesture *gesture-names*))
                        gesture
                        (multiple-value-list (realize-gesture-spec :keyboard gesture))))
           (in-table (position gesture keystroke-accelerators :test #'equal)))
      (when (and in-table errorp)
        (error 'command-already-present :command-table-name
               (command-table-designator-as-name command-table)))
      (if in-table
	  (setf (nth in-table keystroke-items) item)
	  (progn
	    (push gesture keystroke-accelerators)
	    (push item keystroke-items))))))

;; FIXME: According to the spec, we need to remove the menu item if already
;; present. Also, you could argue we don't signal 'command-already-present
;; in quite the right circumstance (see above).
(defun add-keystroke-to-command-table (command-table gesture type value
				       &key documentation (errorp t))
  (let ((command-table (find-command-table command-table)))
    (%add-keystroke-item command-table
                         gesture
			 (make-instance '%menu-item
					:type type :value value
					:keystroke gesture
					:documentation documentation)
			 errorp)))

(defun remove-keystroke-from-command-table (command-table gesture
					    &key (errorp t))
  (let ((command-table (find-command-table command-table)))
    (with-slots (keystroke-accelerators keystroke-items)
	command-table
      (let ((in-table (position gesture keystroke-accelerators :test #'equal)))
	(if in-table
	    (if (zerop in-table)
		(setq keystroke-accelerators (cdr keystroke-accelerators)
		      keystroke-items (cdr keystroke-items))
		(let ((accel-tail (nthcdr (1- in-table)
					  keystroke-accelerators))
		      (items-tail (nthcdr (1- in-table) keystroke-items)))
		  (setf (cdr accel-tail) (cddr accel-tail))
		  (setf (cdr items-tail) (cddr items-tail))))
	    (when errorp
	      (error 'command-not-present :command-table-name
                     (command-table-designator-as-name command-table)))))))
  nil)

(defun map-over-command-table-keystrokes (function command-table)
  (let ((command-table (find-command-table command-table)))
    (with-slots (keystroke-accelerators keystroke-items)
	command-table
      (loop for gesture in keystroke-accelerators
	    for item in keystroke-items
	    do (funcall function
                        (and (slot-boundp item 'menu-name)
                             (command-menu-item-name item))
			gesture
			item)))))

(defun find-keystroke-item (gesture command-table
			    &key (test #'event-matches-gesture-name-p)
			    (errorp t))
  (let ((command-table (find-command-table command-table)))
    (loop for keystroke in (slot-value command-table 'keystroke-accelerators)
	  for item in (slot-value command-table 'keystroke-items)
	  if (funcall test gesture keystroke)
	  do (return-from find-keystroke-item (values item command-table)))
    (if errorp
	(error 'command-not-present :command-table-name
               (command-table-designator-as-name command-table))
	nil)))

(defun lookup-keystroke-item (gesture command-table
			      &key (test #'event-matches-gesture-name-p))
  (let ((command-table (find-command-table command-table)))
    (multiple-value-bind (item table)
	(find-keystroke-item gesture command-table :test test :errorp nil)
      (when table
	(return-from lookup-keystroke-item (values item table)))
      (map-over-command-table-menu-items
       #'(lambda (name keystroke item)
	   (declare (ignore name keystroke))
	   (when (eq (command-menu-item-type item) :menu)
	     (multiple-value-bind (sub-item sub-command-table)
		 (lookup-keystroke-item gesture
					(command-menu-item-value item)
					:test test)
	       (when sub-command-table
		 (return-from lookup-keystroke-item
		   (values sub-item sub-command-table))))))
       command-table))))

(defun partial-command-from-name (command-name command-table)
  (let ((parser (gethash command-name *command-parser-table*)))
    (if (null parser)
        (error 'command-not-present :command-table-name
               (command-table-designator-as-name command-table))
        (cons command-name
              (mapcar #'(lambda (foo)
                          (declare (ignore foo))
                          *unsupplied-argument-marker*)
                      (required-args parser))))))

;;; XXX The spec says that GESTURE may be a gesture name, but also that the
;;; default test is event-matches-gesture-name-p.  Uh...

(defun lookup-keystroke-command-item (gesture command-table
				      &key test (numeric-arg 1))
  (let ((item (lookup-keystroke-item
	       gesture command-table
	       :test (or test
			 #'(lambda (gesture gesture-name)
			     (or (equal gesture gesture-name)
				 (event-matches-gesture-name-p
				  gesture
				  gesture-name)))))))
    (if item
	(let* ((value (command-menu-item-value item))
	       (command (case (command-menu-item-type item)
                          (:command
			  value)
			 (:function
			  (funcall value gesture numeric-arg))
			 ;; XXX What about the :menu case?
			 (otherwise nil))))
	  (if command
              ; Return a literal command, or create a partial command from a command-name
	      (substitute-numeric-argument-marker (if (symbolp command)
                                                      (partial-command-from-name command command-table)
                                                      command)
                                                  numeric-arg)
	      gesture))
	gesture)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Commands

(defclass command-parsers ()
  ((parser :accessor parser :initarg :parser)
   (partial-parser :accessor partial-parser :initarg :partial-parser)
   (argument-unparser :accessor argument-unparser
		      :initarg :argument-unparser)
   (required-args :accessor required-args :initarg :required-args)
   (keyword-args :accessor keyword-args :initarg :keyword-args))

  (:documentation "A container for a command's parsing functions and
  data for unparsing"))

(defvar *command-name-delimiters* '(command-delimiter))

(defvar *command-argument-delimiters* '(command-delimiter))

;;; A type indicating empty input. For example, if one types <space>
;;; to get the default value of a keyword argument, and then types
;;; <return>, we don't want to see "None" in the output history. So,
;;; we define this subtype of null that has no output. It's not meant
;;; to be read if the form is ever accepted again.

(define-presentation-type empty ()
  :inherit-from 'null)

(define-presentation-method present
    (object (type empty) stream (view textual-view) &key &allow-other-keys)
  (declare (ignore object stream)))


(defun accept-form-for-argument (stream arg)
  (let ((accept-keys '(:default :default-type :display-default
		       :prompt :documentation :insert-default)))
    (destructuring-bind (name ptype &rest key-args
			 &key (mentioned-default nil mentioned-default-p)
			 &allow-other-keys)
	arg
      (declare (ignore name))
      `(accept ,ptype :stream ,stream
	       ,@(loop for (key val) on key-args by #'cddr
		       when (member key accept-keys)
		       append `(,key ,val) into args
		       finally (return (if mentioned-default-p
					   `(:default ,mentioned-default
					     ,@args)
					   args)))))))

;;; In the partial command reader accepting-values dialog, default
;;; values come either from the input command arguments, if a value
;;; was supplied, or from the default option for the command argument.
;;;
;;; accept for the partial command reader.  Can this be refactored to
;;; share code with accept-form-for-argument? Probably not.
;;;
;;; original-command-arg is value entered by the user, or
;;; *unsupplied-argument-marker*. command-arg is the current value for the
;;; argument, originally bound to original-command-arg and now possibly
;;; changed by the user.
(defun accept-form-for-argument-partial (stream ptype-arg command-arg
					 original-command-arg )
  (let ((accept-keys '(:default :default-type :display-default
		       :prompt :documentation :insert-default)))
    (destructuring-bind (name ptype &rest key-args)
	ptype-arg
      (declare (ignore name))
      (let ((args (loop
		     for (key val) on key-args by #'cddr
		     if (eq key :default)
		       append `(:default (if (eq ,command-arg
						 *unsupplied-argument-marker*)
					     ,val
					     ,command-arg))
		     else if (member key accept-keys :test #'eq)
		       append `(,key ,val))))
	(setq args (append args `(:query-identifier ',(gensym "COMMAND-PROMPT-ID"))))
	(if (member :default args :test #'eq)
	    `(accept ,ptype :stream ,stream ,@args)
	    `(if (eq ,original-command-arg *unsupplied-argument-marker*)
		 (accept ,ptype :stream ,stream ,@args)
		 (accept ,ptype :stream ,stream :default ,command-arg
			 ,@args)))))))

(defun make-key-acceptors (stream keyword-args key-results)
  ;; We don't use the name as a variable, and we do want a symbol in the
  ;; keyword package.
  (when (null keyword-args)
    (return-from make-key-acceptors nil))
  (setq keyword-args (mapcar #'(lambda (arg)
				 (cons (make-keyword (car arg)) (cdr arg)))
			     keyword-args))
  (let ((key-possibilities (gensym "KEY-POSSIBILITIES"))
	(member-ptype (gensym "MEMBER-PTYPE"))
	(key-result (gensym "KEY-RESULT"))
	(val-result (gensym "VAL-RESULT")))
    `(let ((,key-possibilities nil))
       ,@(mapcar #'(lambda (key-arg)
		     (destructuring-bind (name ptype
					  &key (when t) &allow-other-keys)
			 key-arg
		       (declare (ignore ptype))
		       (let ((key-arg-name (concatenate
					    'string
					    ":"
					    (keyword-arg-name-from-symbol
					     name))))
			 `(when ,when
			    (push `(,,key-arg-name ,,name)
				  ,key-possibilities)))))
		 keyword-args)
       (setq ,key-possibilities (nreverse ,key-possibilities))
       (when ,key-possibilities
	 (input-editor-format ,stream "(keywords) ")
	 (let ((,member-ptype `(token-or-type ,,key-possibilities empty)))
	   (loop
	     (let* ((,key-result (prog1 (accept ,member-ptype
                                                :stream ,stream
                                                :prompt nil
						:default nil)
                                   (eat-delimiter-or-activator)))
		    (,val-result
		     (case ,key-result
		       ,@(mapcar
			  #'(lambda (key-arg)
			      `(,(car key-arg)
				,(accept-form-for-argument stream
							   key-arg)))
			  keyword-args))))
	       (setq ,key-results (list* ,key-result
					 ,val-result
					 ,key-results)))
	     (eat-delimiter-or-activator))))

       ,key-results)))

(defun make-argument-accept-fun (name required-args keyword-args)
  (let ((stream-var (gensym "STREAM"))
	(required-arg-names (mapcar #'car required-args))
	(key-results (gensym "KEY-RESULTS")))
    `(defun ,name (,stream-var)
       (let (,@(mapcar #'(lambda (arg)
			   `(,arg *unsupplied-argument-marker*))
		       required-arg-names)
	       (,key-results nil))
	 (block activated
	   (flet ((eat-delimiter-or-activator ()
		    (let ((gesture (read-gesture :stream ,stream-var)))
		      (when (or (null gesture)
				(activation-gesture-p gesture))
			(return-from activated nil))
		      (unless (delimiter-gesture-p gesture)
			(unread-gesture gesture
					:stream ,stream-var)))))
             (declare (ignorable (function eat-delimiter-or-activator)))
	     (let ((gesture (read-gesture :stream ,stream-var
					  :timeout 0
					  :peek-p t)))
	       (cond ((and gesture (activation-gesture-p gesture))
		      (return-from activated nil)))
	       ,@(mapcan #'(lambda (arg)
			     (copy-list
			      `((setq ,(car arg)
				 ,(accept-form-for-argument stream-var
							    arg))
				(eat-delimiter-or-activator))))
			 required-args)
	       ,(make-key-acceptors stream-var keyword-args key-results))))
	 (list* ,@required-arg-names ,key-results)))))

(defun make-partial-parser-fun (name required-args)
  (with-gensyms (command-table stream label partial-command
		 command-name command-line-name)
    (let* ((required-arg-names (mapcar #'car required-args))
	   (original-args (mapcar #'(lambda (arg)
				      (gensym (format nil "~A-ORIGINAL"
						      (symbol-name arg))))
				  required-arg-names)))
      ;; We don't need fresh gensyms of these variables for each accept form.
      (with-gensyms (value ptype changedp)
	`(defun ,name (,command-table ,stream ,partial-command)
           (do ((still-missing nil t))
               (nil)
             (destructuring-bind (,command-name ,@original-args)
                 ,partial-command
               (let* ((,command-line-name (command-line-name-for-command
                                           ,command-name
                                           ,command-table
                                           :errorp nil))
                      (,label (format nil
                                      "You are being prompted for arguments to ~S"
                                      ,command-line-name))
                      ,@(mapcar #'list required-arg-names original-args))
                 (accepting-values (,stream :select-first-query t :align-prompts t :label ,label)
                   ,@(loop
                        for var in required-arg-names
                        for original-var in original-args
                        for parameter in required-args
                        for first-arg = t then nil
                        append `((multiple-value-bind (,value ,ptype ,changedp)
                                     ,(accept-form-for-argument-partial
                                       stream parameter var original-var)
                                   (declare (ignore ,ptype))
                                   ,@(unless first-arg `((terpri ,stream)))
                                   (when ,changedp
                                     (setq ,var ,value)))))
                   (when still-missing
                     (format ,stream
                             "~&Please supply all arguments.~%")))
                 (setf ,partial-command (list ,command-name ,@required-arg-names))
                 (unless (partial-command-p ,partial-command)
                   (return ,partial-command))))))))))

;;; XXX What do to about :acceptably? -- moore
(defun make-unprocessor-fun (name required-args key-args)
  (with-gensyms (command command-args stream key key-arg-val seperator arg-tail)
    ;; Bind the argument variables because expressions in the
    ;; following arguments (including the presentation type!) might
    ;; reference them.
    (let ((required-arg-bindings nil)
	  (key-case-clauses nil))
      (loop
	 for (arg ptype-form) in required-args
	 collect `(,arg (progn
			  (write-char ,seperator ,stream)
			  (present (car ,command-args) ,ptype-form
				   :stream ,stream)
			  (pop ,command-args)))
	   into arg-bindings
	 finally (setq required-arg-bindings arg-bindings))
      (loop
	 for (arg ptype-form) in key-args
	 for arg-key = (make-keyword arg)
	 collect `(,arg-key

		   (format ,stream "~C:~A~C"
			   ,seperator
			   ,(keyword-arg-name-from-symbol arg)
			   ,seperator)
		   (present ,key-arg-val ,ptype-form
				       :stream ,stream))
	   into key-clauses
	 finally (setq key-case-clauses key-clauses))
      `(defun ,name (,command ,stream)
	 ,(declare-ignorable-form* stream)
	 (let* ((,seperator #\Space) (,command-args (cdr ,command))
		,@required-arg-bindings)
	   (declare (ignorable ,seperator ,command-args
			       ,@(mapcar #'car required-arg-bindings )))
	   ,@(when key-args
		   `((loop
			for ,arg-tail on ,command-args by #'cddr
			for (,key ,key-arg-val) = ,arg-tail
			do (progn
			     (case ,key
			       ,@key-case-clauses)
			     (when (cddr ,arg-tail)
			       (write-char ,seperator ,stream)))))))))))

(defun make-command-translators (command-name command-table args)
  "Helper function to create command presentation translators for a command."
  (loop with readable-command-name = (command-name-from-symbol command-name) ; XXX or :NAME
        for arg in args
	for arg-index from 0
	append (when (getf (cddr arg) :gesture)
		 (destructuring-bind (name ptype
				      &key gesture &allow-other-keys)
		     arg
                   (declare (ignore name))
		   (let ((command-args (loop for a in args
					     for i from 0
					     if (eql i arg-index)
					       collect 'object
					     else
					       collect (getf (cddr a) :default)
					     end))
			 (translator-name (intern (format nil
							  ".~A-ARG~D."
							  command-name
							  arg-index)
						  (symbol-package command-name))))
		     (multiple-value-bind (gesture translator-options)
			 (if (listp gesture)
			     (values (car gesture) (cdr gesture))
			     (values gesture nil))
                       (destructuring-bind (&key (documentation
                                                  `((object stream)
                                                    (orf stream *standard-output*)
                                                    (format stream "~A "
                                                            ,readable-command-name)
                                                    (present object (presentation-type-of object) ; type?
                                                             :stream stream
                                                             :acceptably nil
                                                             :sensitive nil))
                                                  documentationp)
                                            &allow-other-keys)
                           translator-options
		       `(define-presentation-to-command-translator
			    ,translator-name
			    (,(eval ptype) ,command-name ,command-table
			     :gesture ,gesture
                             ,@(unless documentationp `(:documentation ,documentation))
			     ,@translator-options)
			  (object)
			  (list ,@command-args)))))))))

;;; Vanilla define-command, as defined by the standard
(defmacro %define-command (name-and-options args &body body)
  (unless (listp name-and-options)
    (setq name-and-options (list name-and-options)))
  (destructuring-bind (func &key command-table name menu keystroke)
      name-and-options
    (multiple-value-bind (required-args keyword-args)
	(loop for arg-tail on args
	      for (arg) = arg-tail
	      until (eq arg '&key)
	      collect arg into required
	      finally (return (values required (cdr arg-tail))))
      (let* ((command-func-args
	      `(,@(mapcar #'car required-args)
		,@(and
		   keyword-args
		   `(&key ,@(mapcar #'(lambda (arg-clause)
					(destructuring-bind (arg-name ptype
							     &key default
							     &allow-other-keys)
					    arg-clause
					  (declare (ignore ptype))
					  `(,arg-name ,default)))
				    keyword-args)))))
	     (accept-fun-name (gentemp (format nil "~A%ACCEPTOR%"
					       (symbol-name func))
				       (symbol-package func)))
	     (partial-parser-fun-name (gentemp (format nil "~A%PARTIAL%"
						       (symbol-name func))
					       (symbol-package func)))
	     (arg-unparser-fun-name (gentemp (format nil "~A%unparser%"
						     (symbol-name func))
					     (symbol-package func))))
	`(progn
	  (defun ,func ,command-func-args
	    ,@body)
	  ,(when command-table
                 `(add-command-to-command-table ',func ',command-table
		 :name ,name :menu ',menu
		 :keystroke ',keystroke :errorp nil
		 ,@(and menu
			`(:menu-command
			  (list ',func
			        ,@(make-list (length required-args)
					     :initial-element
					     '*unsupplied-argument-marker*))))))
	  ,(make-argument-accept-fun accept-fun-name
				     required-args
				     keyword-args)
	  ,(make-partial-parser-fun partial-parser-fun-name required-args)
	  ,(make-unprocessor-fun arg-unparser-fun-name
				 required-args
				 keyword-args)
	  ,(and command-table
		(make-command-translators func command-table required-args))
	  (setf (gethash ',func *command-parser-table*)
	        (make-instance 'command-parsers
		               :parser #',accept-fun-name
		               :partial-parser #',partial-parser-fun-name
                               :required-args ',required-args
                               :keyword-args  ',keyword-args
			       :argument-unparser #',arg-unparser-fun-name))
	  ',func)))))

;;; Output destination extension for DEFINE-COMMAND
;;;
;;; The backend part, i.e. INVOKE-WITH-STANDARD-OUTPUT and the
;;; destination classes, is defined in clim-basic/stream-output.lisp.

(define-presentation-method accept
    ((type stream-destination) stream (view textual-view)
                               &key
                               (default '*standard-output*)
                               (prompt "stream form"))
  (let ((dest (eval (accept 'form :stream stream :view view
                                  :default default :prompt prompt))))
    (if (and (streamp dest)
             (output-stream-p dest))
        (make-instance 'stream-destination :destination-stream dest)
        (input-not-of-required-type dest type))))

(define-presentation-method accept
    ((type file-destination) stream (view textual-view)
                             &key (prompt "destination file"))
  (let ((path (accept 'pathname :stream stream :view view :prompt prompt)))
    ;; Give subclasses a shot
    (with-presentation-type-decoded (type-name) type
      (make-instance type-name :file path))))

(define-presentation-method accept
    ((type output-destination) stream (view textual-view)
                               &key
                               (default "Stream")
                               (prompt nil))
  (let ((type (accept `(member-alist ,*output-destination-types*)
                      :stream stream :view view
                      :default default :prompt prompt
                      :additional-delimiter-gestures '(#\space))))
    (read-char stream)
    (accept type :stream stream :view view :prompt nil)))

;;; The default for :provide-output-destination-keyword is nil until we fix
;;; some unfortunate problems with completion, defaulting, and keyword
;;; arguments.

(defmacro define-command (name-and-options args &body body)
  (unless (listp name-and-options)
    (setq name-and-options (list name-and-options)))
  ;; According to the specification all argument description elements except the
  ;; parameter name are evaluated. We lax this requirement a little and evaluate
  ;; only type specifier *if* it is a list. Atom types are not evaluated to
  ;; reasemble method specialization. Moreover we validate here argument
  ;; description keyword arguments in destructuring-bind (as suggested by Xof in
  ;; the spec annotation we require keywords being macroexpand-time
  ;; constant). We allow two custom key arguments, but we should fix ESA
  ;; instead. -- jd 2018-09-14
  (mapc (lambda (argument-description)
          (unless (eq argument-description '&key)
            ;; Ensure correct structure and valid keywords.
            (destructuring-bind (parameter type &key
                                 default default-type display-default mentioned-default
                                 prompt documentation when gesture
                                 ;; These two are not standard, but ESA uses them.
                                 prompt-mode insert-default)
                argument-description
              (declare (ignore parameter default default-type display-default mentioned-default
                               prompt documentation when gesture
                               prompt-mode insert-default))
              ;; Quote atomic types to reassemble defmethod more.
              (when (atom type)
                (setf (second argument-description) `(quote ,type))))))
        args)
  (destructuring-bind (func &rest options
                       &key (provide-output-destination-keyword nil)
                       &allow-other-keys)
      name-and-options
    (with-keywords-removed (options (:provide-output-destination-keyword))
      (if provide-output-destination-keyword
          (let* ((key-supplied (find '&key args))
                 (destination-arg '(output-destination 'output-destination
                                    :default nil :display-default nil))
                 (new-args (if key-supplied
                               `(,@args ,destination-arg)
                               `(,@args &key ,destination-arg))))
            (multiple-value-bind (decls new-body)
                (get-body-declarations body)
              (with-gensyms (destination-continuation)
                `(%define-command (,func ,@options) ,new-args
                   ,@decls
                   (flet ((,destination-continuation ()
                            ,@new-body))
                     (declare (dynamic-extent #',destination-continuation))
                     (invoke-with-standard-output #',destination-continuation
                                                  output-destination))))))
          `(%define-command (,func ,@options)
                            ,args
             ,@body)))))

;;; Note that command table inheritance is the opposite of Common Lisp
;;; subclassing / subtyping: the inheriting table defines a superset
;;; of the commands of its ancestor, so therefore it's command
;;; presentation type is a supertype of its ancestor's!
(defun command-table-inherits-from-p (command-table super-table)
  (let ((command-table (find-command-table command-table))
	(super-table (find-command-table super-table)))
    (do-command-table-inheritance (table command-table)
      (when (eq table super-table)
	(return-from command-table-inherits-from-p (values t t))))
    (values nil t)))

(define-presentation-type command-name
    (&key (command-table (frame-command-table *application-frame*)))
  :inherit-from t)

(define-presentation-method presentation-typep (object (type command-name))
  (and (command-accessible-in-command-table-p object command-table)
       (command-enabled object *application-frame*)))

(define-presentation-method presentation-subtypep ((type command-name)
						   maybe-supertype)
  (with-presentation-type-parameters (command-name maybe-supertype)
    (let ((super-table command-table))
      (with-presentation-type-parameters (command-name type)
	(command-table-inherits-from-p super-table command-table)))))

(define-presentation-method present (object (type command-name)
				     stream
				     (view textual-view)
				     &key)
  (let ((command-line-name (command-line-name-for-command object command-table
							  :errorp nil)))
    (if command-line-name
	(write-string command-line-name stream)
	(prin1 object stream))))

(define-presentation-method accept ((type command-name) stream
				    (view textual-view)
				    &key)
  (flet ((generator (string suggester)
	   (declare (ignore string))
           (let ((possibilities nil))
             (map-over-command-table-names
              (lambda (cline-name command-name)
                (unless (member command-name (disabled-commands *application-frame*))
                  (pushnew (cons cline-name command-name) possibilities
                           :key #'car :test #'string=)))
              command-table)
             (loop for (cline-name . command-name) in possibilities
                   do (funcall suggester cline-name command-name)))))
    ;; KLUDGE: here, we used to bind the frame's command table so that
    ;; a test with COMMAND-ENABLED passed with the command-table being
    ;; accepted from.  Unfortunately, that interfered awfully with
    ;; drei gadgets and their command-table inheritance; the dynamic
    ;; inheritance from (frame-command-table *application-frame*) [
    ;; which is needed to get things like frame menu items and other
    ;; commands to work ] works really badly if (frame-command-table
    ;; *application-frame*) is set/bound to the dispatching
    ;; command-table itself.
    ;;
    ;; Instead we now use the knowledge of how disabled commands are
    ;; implemented to satisfy the constraint that only enabeled
    ;; commands are acceptable (with the "accessible" constraint being
    ;; automatically satisfied by the generator mapping over the
    ;; command-table).
    ;;
    ;; This means that someone implementing their own version of the
    ;; "enabled-command" protocol will lose.  Sorry.  CSR, 2009-02-17
    (multiple-value-bind (object success string)
	(complete-input stream
			#'(lambda (so-far mode)
			    (complete-from-generator so-far
						     #'generator
						     '(#\space)
						     :action mode))
			:partial-completers '(#\space))
      (if success
	  (values object type)
	  (simple-parse-error "No command named ~S" string)))))

(defun command-line-command-parser (command-table stream)
  (let ((command-name nil)
	(command-args nil))
    (with-delimiter-gestures (*command-name-delimiters* :override t)
      ;; While reading the command name we want use the history of the
      ;; (accept 'command ...) that's calling this function.
      (setq command-name (accept `(command-name :command-table ,command-table)
				 :stream stream :prompt nil :history nil))
      (let ((delimiter (read-gesture :stream stream :peek-p t)))
	;; Let argument parsing function see activation gestures.
	(when (and delimiter (delimiter-gesture-p delimiter))
	  (read-gesture :stream stream))))
    (with-delimiter-gestures (*command-argument-delimiters* :override t)
      (setq command-args (funcall (parser (gethash command-name
						   *command-parser-table*))
				  stream)))
    (cons command-name command-args)))

(defun command-line-command-unparser (command-table stream command)
  (write-string (command-line-name-for-command (car command) command-table
					       :errorp :create)
		stream)
  (when (cdr command)
    (let ((parser-obj (gethash (car command) *command-parser-table*)))
      (if parser-obj
	  (funcall (argument-unparser parser-obj) command stream)
	    (with-delimiter-gestures (*command-argument-delimiters*
				      :override t)
	      (loop for arg in (cdr command)
		 do (progn
		      (write-char #\space stream)
		      (write-token
		       (present-to-string arg
					  (presentation-type-of arg))
		       stream))))))))

;;; In order for this to work, the input-editing-stream must implement
;;; a method for the nonstandard function
;;; `input-editing-stream-output-record'.
(defun command-line-read-remaining-arguments-for-partial-command
    (command-table stream partial-command start-position)
  (declare (ignore start-position))
  (let ((partial-parser (partial-parser (gethash (command-name partial-command)
						 *command-parser-table*))))
    (if (encapsulating-stream-p stream)
	(let ((interactor (encapsulating-stream-stream stream)))
	  (with-bounding-rectangle* (x1 y1 x2 y2) (input-editing-stream-output-record stream)
	    (declare (ignore y1 x2))
	    ;; Start the dialog below the editor area
	    (letf (((stream-cursor-position interactor) (values x1 y2)))
	      (fresh-line interactor)
	      ;; FIXME error checking needed here? -- moore
	      (funcall partial-parser
		       command-table interactor partial-command))))
	(progn
	  (fresh-line stream)
	  (funcall partial-parser
		 command-table stream  partial-command)))))

(defparameter *command-parser* #'command-line-command-parser)

(defparameter *command-unparser* #'command-line-command-unparser)

(defvar *partial-command-parser*
  #'command-line-read-remaining-arguments-for-partial-command)

(define-presentation-type command
    (&key (command-table (frame-command-table *application-frame*)))
  :inherit-from t)

(define-presentation-method presentation-typep (object (type command))
  (and (consp object)
       (presentation-typep (car object)
			   `(command-name :command-table ,command-table))))

(define-presentation-method presentation-subtypep ((type command)
						   maybe-supertype)
  (with-presentation-type-parameters (command maybe-supertype)
    (let ((super-table command-table))
      (with-presentation-type-parameters (command type)
	(command-table-inherits-from-p super-table command-table)))))

(define-presentation-method present (object (type command)
				     stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (funcall *command-unparser* command-table stream object))

(define-presentation-method accept ((type command) stream
				    (view textual-view)
				    &key)
  (let ((command (funcall *command-parser* command-table stream)))
    (cond ((null command)
	   (simple-parse-error "Empty command"))
          ((partial-command-p command)
           (funcall *partial-command-parser*
            command-table stream command
            (position *unsupplied-argument-marker* command)))
	  (t (values command type)))))

;;; A presentation type for empty input at the command line; something for
;;; read-command to supply as a default.  The command is defined in
;;; builtin-commands.lisp.

(define-presentation-type null-command
    ()
  :inherit-from '(command :command-table global-command-table))

(define-presentation-method presentation-typep (object (type null-command))
  (and (consp object) (eq (car object) 'com-null-command)))

(define-presentation-method present
    (object (type null-command) stream (view textual-view) &key)
  (declare (ignore object stream view)))

(defparameter +null-command+ '(com-null-command))

(defun command-name (command)
  (first command))

(defun command-arguments (command)
  (rest command))

(defun partial-command-p (command)
  (member *unsupplied-argument-marker* command))

(defmacro with-command-table-keystrokes ((keystroke-var command-table)
					 &body body)
  (with-gensyms (table)
    `(let* ((,table (find-command-table ,command-table))
	    (,keystroke-var (slot-value ,table 'keystroke-accelerators)))
       ,@body)))

;; This is probably wrong - we should walk the menu rather than inheritance
;; structure to be consistent with lookup-keystroke-item.
(defun compute-inherited-keystrokes (command-table)
  "Return a list containing the keyboard gestures of accelerators defined in
 'command-table' and all tables it inherits from."
  (let (accumulated-keystrokes)
    (do-command-table-inheritance (comtab command-table)
      (with-command-table-keystrokes (keystrokes comtab)
        (dolist (keystroke keystrokes)
          (setf accumulated-keystrokes (adjoin keystroke accumulated-keystrokes :test #'equal)))))
    accumulated-keystrokes))

(defun read-command (command-table
		     &key (stream *standard-input*)
			  (command-parser *command-parser*)
			  (command-unparser *command-unparser*)
			  (partial-command-parser *partial-command-parser*)
			  use-keystrokes)
  (let ((*command-parser* command-parser)
	(*command-unparser* command-unparser)
	(*partial-command-parser* partial-command-parser))
    (cond (use-keystrokes
	   (let ((stroke-result
                  (read-command-using-keystrokes command-table
                                                 (compute-inherited-keystrokes command-table)
                                                 :stream stream)))
	     (if (consp stroke-result)
		 stroke-result
		 nil)))
	  ((or (typep stream 'interactor-pane)
	       (typep stream 'input-editing-stream))
	   (handler-case
	       (multiple-value-bind (command ptype)
		   (accept `(command :command-table ,command-table)
			   :stream stream
			   :prompt nil
			   :default +null-command+
			   :default-type 'null-command)
		 (cond ((eq ptype 'null-command)
			nil)
		       ((partial-command-p command)
			(beep)
			(format *query-io* "~&Argument ~D not supplied.~&"
				(position *unsupplied-argument-marker* command))
			nil)
		       (t command)))
	     ((or simple-parse-error input-not-of-required-type)  (c)
	       (beep)
	       (fresh-line *query-io*)
	       (princ c *query-io*)
	       (terpri *query-io*)
	       nil)))
	  (t (with-input-context (`(command :command-table ,command-table))
	       (object)
	       (loop (read-gesture :stream stream))
	       (t object))))))


(defun read-command-using-keystrokes (command-table keystrokes
				      &key (stream *standard-input*)
				      (command-parser *command-parser*)
				      (command-unparser *command-unparser*)
				      (partial-command-parser *partial-command-parser*))
  (let ((*command-parser* command-parser)
	(*command-unparser* command-unparser)
	(*partial-command-parser* partial-command-parser)
	(*accelerator-gestures* keystrokes))
    (handler-case (read-command command-table :stream stream)
      (accelerator-gesture (c)
        ;; If lookup-keystroke-item below returns a partial command, invoke the
        ;; partial command parser to complete it.
        (let ((command
               (lookup-keystroke-command-item (accelerator-gesture-event c)
                                              command-table)))
          (if (and (listp command)
                   (partial-command-p command))
              (funcall *partial-command-parser*
                       command-table stream command
                       (position *unsupplied-argument-marker* command))
              command))))))

(defun substitute-numeric-argument-marker (command numeric-arg)
  (substitute numeric-arg *numeric-argument-marker* command))

(defvar *command-dispatchers* '(#\:))

(define-presentation-type command-or-form
    (&key (command-table (frame-command-table *application-frame*)))
  :inherit-from t)

;;; What's the deal with this use of with-input-context inside of
;;; accept? When this accept method is called, we want to accept both
;;; commands and forms via mouse clicks, both before and after the
;;; command dispatch character is typed. But command translators to
;;; command or form won't be applicable... translators from command or
;;; form to command-or-form won't help either because translators aren't
;;; applied more than once.
;;;
;;; By calling the input context continuation directly -- which was
;;; established by the call to (accept 'command-or-form ...) -- we let it do
;;; all the cleanup like replacing input, etc.

(define-presentation-method accept ((type command-or-form) stream
				    (view textual-view)
				    &key)
  (let ((command-ptype `(command :command-table ,command-table)))
    (with-input-context (`(or ,command-ptype form))
        (object type event options)
        (let ((initial-char (read-gesture :stream stream :peek-p t)))
	  (if (member initial-char *command-dispatchers*)
	      (progn
		(read-gesture :stream stream)
		(accept command-ptype :stream stream :view view :prompt nil :history 'command))
	      (accept 'form :stream stream :view view :prompt nil :history 'command-or-form)))
      (t
       (funcall (cdar *input-context*) object type event options)))))
