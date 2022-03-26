;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLIM-XCOMMON; -*-
;;; --------------------------------------------------------------------------------------
;;;     Title: X11 keysym handling
;;;   Created: 2002-02-11
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: LGPL (See file COPYING for details).
;;; --------------------------------------------------------------------------------------
;;;  (c) copyright 2002 by Gilbert Baumann

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

;;; Support and port mixin for X based backends, handling keycode to
;;; keysym and character mapping, and handling of modifiers.

(in-package :clim-xcommon)

;;; Recall some terminology.  A KEYCODE is an integer between 8 and
;;; 255 and it corresponds to some key on the keyboard.  The
;;; correspondence is arbitrary in that it can vary from one X11
;;; server to another.
;;;
;;; A KEYSYM is non-negative integer with an equivalent Common Lisp
;;; type of (UNSIGNED-BYTE 32).  A keysym has a fixed meaning as a
;;; some upper or lower-case letter, as some special character, or as
;;; some operation that does not have a character equivalent, for
;;; example the UP arrow key or the CapsLock key.
;;;
;;; In addition, we give one or more NAMEs to some important keysyms.
;;; Such a KEYSYM NAME is a symbol in the KEYWORD package.  Even
;;; ordinary letters follow this pattern, so that the keysym name for
;;; the lower-case letter #\a is the symbol :|a|.
;;;
;;; Let us introduce some terminology related to modifiers.  A
;;; MODIFIER is some abstract concept with no representation in code.
;;; Examples of modifiers are SHIFT, CONTROL, CAPSLOCK, HYPER, etc.  A
;;; MODIFIER KEYCODE is a keycode that is currently working as a
;;; modifier.  A MODIFIER KEYSYM is one of the keysyms in the fixed
;;; set of keysyms corresponding to modifier keys.  A MODIFIER NAME is
;;; the keysym name of modifier keysym.  A MODIFIER VALUE is an
;;; integer that is a power of 2 and that uniquely identifies a
;;; particular modifier.  A MODIFIER MASK is an integer made up the
;;; logical OR of modifier values.

;;; This hash table maps a keysym to a list of keysym names for that
;;; keysym.
(defvar *keysym-name-table*
  (make-hash-table :test #'eql))

;;; This hash table maps a keysym name to the corresponding keysym.
(defvar *keysym-table*
  (make-hash-table :test #'eq))

(defun define-keysym (name value)
  (pushnew name (gethash value *keysym-name-table* nil))
  (setf (gethash name *keysym-table*) value))

(defun keysym-to-keysym-name (value)
  (car (last (gethash value *keysym-name-table*))))

(defun keysym-name-to-keysym (value)
  (gethash value *keysym-table*))

(defclass keysym-port-mixin ()
  ((modifier-cache :accessor modifier-cache :initform nil)))

(defgeneric modifier-mapping (port)
  (:documentation "Returns an array of lists of keysym keywords for
  each of the X modifiers shift lock control mod1 mod2 mod3 mod4 mod5"))

(defmethod modifier-mapping (port)
  (error "Define me!"))

;;; The X state is the state before the current event, so key events
;;; for the modifier keys don't reflect the state that results from
;;; pressing or releasing those keys.  We want the CLIM modifiers to
;;; reflect the post event state.

(defun x-keysym-to-clim-modifiers (port event-key keychar keysym-name state)
  "Return modifiers for PORT with STATE.
   If KEYCHAR is a special key(like shift, caps-lock, etc.), update
   the modifiers cache. EVENT-KEY is :key-press or :key-release"
  (multiple-value-bind (clim-modifiers caps-lock? mode-switch?)
      (x-event-state-modifiers port state)
    (declare (ignore caps-lock? mode-switch?))
    (if (characterp keychar)
        clim-modifiers
        (modify-modifiers event-key keysym-name clim-modifiers))))

;;; Modifier cache
;;;
;;; A cache entry is a CONS of two bit masks, each one represented as
;;; an integer.  Each bit mask is the logical OR of constants, each of
;;; which is a power of 2 and defining some modifier.
;;;
;;; The CAR of the cache entry is the bit mask for modifiers defined
;;; by the CLIM II specification.  The corresponding constants are not
;;; backend specific, so they are defined elsewhere.
;;;
;;; The CDR of the cache entry is the bit mask for backend-specific
;;; modifiers, in this case CLX modifiers for shift lock, caps lock,
;;; and mode switch.  Recall that the mode switch modifier is the one
;;; that determines which of the two groups of keysyms should be used
;;; as an interpretation of a particular keycode.

;;; Definition of constants for the backend-specific modifier mask.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +caps-lock+ 1)
  (defconstant +mode-switch+ 4))

;;; This dictionary maps CLX keysym names to the power-of-two
;;; constants that the CLIM II specification requires.
(defconstant +clim-modifiers+
  ;; TODO We treat both alt keys as +META-KEY+ (instead of +ALT-KEY+)
  ;; because
  ;; 1) +ALT-KEY+ is non-standard
  ;; 2) +ALT-KEY+ is not exported
  ;; 3) The gesture infrastructure does not accept :alt as a modifier
  '(((:meta-left :meta-right :alt-left :alt-right) #.+meta-key+)
    ((:hyper-left :hyper-right)                    #.+hyper-key+)
    ((:super-left :super-right)                    #.+super-key+)
    ((:shift-left :shift-right)                    #.+shift-key+)
    ((:control-left :control-right)                #.+control-key+)))

;;; This dictionary maps CLX keysym names to the power-of-two
;;; constants that are not required by the CLIM II specification, but
;;; that we need anyway, in order to determine what keysym to choose
;;; based on current modifiers.
(defconstant +other-modifiers+
  '((:caps-lock #.+caps-lock+)
    (:mode-switch #.+mode-switch+)))

;;; We need a way to interpret the individual bits of an X11 modifier
;;; mask.  This is not a trivial thing to do, because X11 uses
;;; different rules for different types of modifiers, and for some
;;; cases there are no fixed rules.
;;;
;;; The three least significant bit positions in a mask have a fixed
;;; interpretation.  Bit 0 means shift, bit 1 means lock and bit 2
;;; means control.  A keycode assigned to one of these positions takes
;;; the meaning of the position, independently of the keycode and the
;;; keysym that the keycode is associated with.
;;;
;;; Some modifiers, notably num-lock and mode switch, work very
;;; differently.  Here, the keysym is important.  To get the effect of
;;; num-lock, there has to be a keycode with the associated keysym
;;; that is specific to this modifier assigned to a bit position.
;;; Similarly, to get the effect of mode switch, there has to be a
;;; keycode with the associated keysym that is specific to this
;;; modifier assigned to a bit position.
;;;
;;; Finally, for some modifiers, there are no specific rules.  The
;;; ones we are particularly interested in are META, SUPER, and HYPER.
;;; So, we have a choice.  We could either use a fixed-position rule.
;;; That solution makes it possible to associate any keycode,
;;; independently of its associated keysym to one of these modifiers.
;;; The other possibility is to use the rule of the associated keysym.
;;; That solution would require the user to change the mapping from
;;; keycodes to keysyms in order to obtain these mappings.
;;;
;;; We are opting for the fixed-position, for the following reasons:
;;; There is a tradition for mod1 to mean ALT or META, and for mod4 to
;;; mean SUPER. Furthermore, mod3 is not assigned to anything in most
;;; default configurations, so we can use it for HYPER.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +shift-bit+      #b00000001)
  (defconstant +lock-bit+       #b00000010)
  (defconstant +control-bit+    #b00000100)
  (defparameter *meta-bit*      #b00001000)
  (defparameter *hyper-bit*     #b00100000)
  (defparameter *super-bit*     #b01000000))

;;; Given an X11/CLX modifier mask, return a CLIM modifier mask with
;;; the relevant bits set.  Recall that the CLIM modifier mask does
;;; not contain bits corresponding to modifiers such as locks or mode
;;; switches.
(defun create-clim-modifier-mask (clx-modifier-mask)
  (let ((m clx-modifier-mask))
    (logior (if (plusp (logand m +shift-bit+)) +shift-key+ 0)
	    (if (plusp (logand m +control-bit+)) +control-key+ 0)
	    (if (plusp (logand m *meta-bit*)) +meta-key+ 0)
	    (if (plusp (logand m *hyper-bit*)) +hyper-key+ 0)
	    (if (plusp (logand m *super-bit*)) +super-key+ 0))))

;;; Return the keysym name for the keysym associated with KEYCODE in
;;; DISPLAY.
(defun code-to-name (keycode display)
  (keysym-to-keysym-name (xlib:keycode->keysym display keycode 0)))

;;; Return the bit position in a modifier mask that should be
;;; interpreted as the MODE-SWITCH modifier, or NIL if no modifier is
;;; to be interpreted as MODE-SWITCH.
(defun mode-switch-position (display)
  (position-if (lambda (keycodes)
		 (find :mode-switch keycodes
		       :key (lambda (keycode) (code-to-name keycode display))))
	       (multiple-value-list (xlib:modifier-mapping display))))

;;; Return the bit position in a modifier mask that should be
;;; interpreted as the NUM-LOCK modifier, or NIL if no modifier is to
;;; be interpreted as NUM-LOCK.
(defun num-lock-position (display)
  (position-if (lambda (keycodes)
		 (find :num-lock keycodes
		       :key (lambda (keycode) (code-to-name keycode display))))
	       (multiple-value-list (xlib:modifier-mapping display))))

(defun position-to-mask (position)
  (if (null position)
      0
      (ash 1 position)))

;;; Return true if and only if the lock modifier should be interpreted
;;; as CAPS-LOCK.
(defun lock-is-caps-lock-p (display)
  (find :caps-lock (nth-value 1 (xlib:modifier-mapping display))
	:key (lambda (keycode) (code-to-name keycode display))))

;;; Given an X11/CLX modifier mask, return a backend-specific modifier
;;; mask with the relevant bits set.
(defun create-other-modifier-mask (clx-modifier-mask
				   caps-lock-mask
				   mode-switch-mask)
  (let ((m clx-modifier-mask))
    (logior (if (plusp (logand m caps-lock-mask)) +caps-lock+ 0)
	    (if (plusp (logand m mode-switch-mask)) +mode-switch+ 0))))

;;; Recall that the function MODIFIER-MAPPING is similar to the one
;;; with the same name in the XLIB package.  It returns a vector of
;;; length 8, where each element is a list of keysym names (which are
;;; Common Lisp symbols in the KEYWORD package).  The elements of the
;;; vector are as follows:
;;;
;;;   index  meaning
;;;     0    shift keysym names
;;;     1    lock keysym names
;;;     2    control keysym names
;;;     3    mod1 keysym names
;;;     4    mod2 keysym names
;;;     5    mod3 keysym names
;;;     6    mod4 keysym names
;;;     7    mod5 keysym names

;;; This definition does not work because it uses keysyms rather than
;;; bit positions.
;; (defun make-modifier-cache (port)
;;   (let* ((modifier-mapping (modifier-mapping port))
;;       ;; This variable holds the number of different possible
;;       ;; modifiers, and the X11 specification says that it will
;;       ;; always be 8.
;;       (modifier-count (length modifier-mapping))
;;       ;; This variable holds the number of different possible
;;       ;; modifier masks, and the X11 specification says that it
;;       ;; will always be 256.
;;       (modifier-mask-count (ash 1 modifier-count))
;;       (cache (make-array modifier-mask-count)))
;;     (loop for x-modifier-mask from 0 below modifier-mask-count
;;        for clim-modifier = 0
;;        for other-modifier = 0
;;        do (loop for bit from 0 below modifier-count
;;                 for modifier-names = (aref modifier-mapping bit)
;;                 when (logbitp bit x-modifier-mask)
;;                   do (loop for (syms val) in +clim-modifiers+
;;                            when (intersection syms modifier-names)
;;                              do (setf clim-modifier
;;                                       (logior clim-modifier val)))
;;                      (loop for (sym val) in +other-modifiers+
;;                            when (member sym modifier-names)
;;                              do (setf other-modifier
;;                                       (logior other-modifier val)))
;;                 finally (setf (aref cache x-modifier-mask)
;;                               (cons clim-modifier other-modifier))))
;;     (setf (modifier-cache port) cache)))

(defun make-modifier-cache (port)
  (let* ((cache (make-array 256))
	 (display (clim-clx::clx-port-display port))
	 (caps-lock-mask (if (lock-is-caps-lock-p display) +lock-bit+ #b00))
	 (mode-switch-position (mode-switch-position display))
	 (mode-switch-mask (position-to-mask mode-switch-position)))
    (loop for x-modifier-mask from 0 below 256
	  do (setf (aref cache x-modifier-mask)
		   (cons (create-clim-modifier-mask x-modifier-mask)
			 (create-other-modifier-mask x-modifier-mask
						     caps-lock-mask
						     mode-switch-mask))))
    cache))

(defgeneric x-event-state-modifiers (port state)
  (:documentation "For the X STATE, returns as multiple values, the
  corresponding set of CLIM modifiers and flags for shift lock, caps lock, and
  mode switch."))

(defmethod x-event-state-modifiers ((port keysym-port-mixin) state)
  (with-accessors ((modifier-cache modifier-cache))
      port
    (unless modifier-cache
      (setf modifier-cache (make-modifier-cache port)))
    (destructuring-bind (clim-modifiers . other-modifiers)
	;; Mask off the button state bits.
	(aref modifier-cache
	      (mod state (length modifier-cache)))
      (values clim-modifiers
	      (logtest +caps-lock+ other-modifiers)
	      (logtest +mode-switch+ other-modifiers)))))

(defun modify-modifiers (event-key keysym-name modifiers)
  (let ((keysym-modifier (loop for (keysyms modifier) in +clim-modifiers+
			       if (member keysym-name keysyms)
			       return modifier)))
    (cond ((and keysym-modifier (eq event-key :key-press))
	   (logior modifiers keysym-modifier))
	  ((and keysym-modifier (eq event-key :key-release))
	   (logandc2 modifiers keysym-modifier))
	  (t modifiers))))
