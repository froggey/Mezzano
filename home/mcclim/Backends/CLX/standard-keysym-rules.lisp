;;;; ----------------------------------------------------------------------
;;;;     Title: X11 standard keysym interpretation rules
;;;;   Created: 2016-01-25
;;;;    Author: Robert Strandh <robert.strandh@gmail.com>
;;;;   License: LGPL (See file COPYING for details).
;;;; ----------------------------------------------------------------------
;;;;  (c) copyright 2016 by Robert Strandh

;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Library General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Library General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Library General Public
;;;; License along with this library; if not, write to the 
;;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, 
;;;; Boston, MA  02111-1307  USA.

(cl:in-package #:clim-clx)

;;;; The purpose of the code in this file is to apply the standard X11
;;;; rules for translating a keycode to a keysym.
;;;;
;;;; Recall that the standard rules for interpreting keycodes involve
;;;; (up to) four different keysyms, indexed from 0 to 3.  Keysyms
;;;; with indices 0 and 1 are said to be in group 1 and keysyms with
;;;; indices 2 and 3 are said to be in group 2.

;;;; MODE SWITCH: Whether keysyms in group 1 or keysyms in group 2
;;;; should be used is controlled by a bit position in the modifier
;;;; mask in effect when a key-press event occurs.  It can be one or
;;;; more of the bits corresponding to Mod1 through Mod5, or none of
;;;; those bits.  Whether one of those bits (say M) indicates that
;;;; group 2 should be used is controlled by a two things: an
;;;; assignment of some keycode K to the keysym named :MODE-SWITCH,
;;;; and the attachment of that same keycode K to the modifier M.  We
;;;; can determine whether this is the case by looking at the return
;;;; values of a call to XLIB:MODIFIER-MAPPING corresponding to Mod1
;;;; through Mod5, and then call XLIB:KEYCODE->KEYSYM with each of the
;;;; keycodes in those return values to check whether it is assigned
;;;; to the keysym named :MODE-SWITCH.  This information needs to be
;;;; determined only at start-up and when the keyboard mapping
;;;; changes.  We summarize this information as a mask that has a 1 in
;;;; the position corresponding to M if and only if M should be
;;;; interpreted as a mode switch modifier.
(defun compute-mode-switch-mask (display)
  ;; Loop over the modifier positions corresponding to Mod1
  ;; through Mod5.
  (loop with mask = #b00000000
	with keysym = (clim-xcommon:keysym-name-to-keysym :MODE-SWITCH)
	for index from 3 to 7
	for keycodes = (nth-value index (xlib:modifier-mapping display))
	do (loop for keycode in keycodes
		 when (= keysym (xlib:keycode->keysym display keycode 0))
		   do (setf mask (logior mask (ash 1 index))))
	finally (return mask)))

;;;; Which keysym to use within a group is controlled by rules
;;;; described below.  To understand those rules, we need to
;;;; understand how to determine from the modifier mask whether
;;;; any of caps-lock, shift-lock, or num-lock is in effect.

;;;; NUM-LOCK: Whether num-lock is in effect is controlled by a bit
;;;; position in the modifier mask in effect when a key-press event
;;;; occurs in a way similar to the way mode switch is determined.  It
;;;; can be one or more of the bits corresponding to Mod1 through
;;;; Mod5, or none of those bits.  Whether one of those bits (say M)
;;;; indicates that num-lock is in effect is controlled by a two
;;;; things: an assignment of some keycode K to the keysym named
;;;; :NUM-LOCK, and the attachment of that same keycode K to the
;;;; modifier M.  Again, We can determine whether this is the case by
;;;; looking at the return values of a call to XLIB:MODIFIER-MAPPING
;;;; corresponding to Mod1 through Mod5, and then call
;;;; XLIB:KEYCODE->KEYSYM with each of the keycodes in those return
;;;; values to check whether it is assigned to the keysym named
;;;; :NUM-LOCK.  This information needs to be determined only at
;;;; start-up and when the keyboard mapping changes.  We summarize
;;;; this information as a mask that has a 1 in the position
;;;; corresponding to M if and only if M should be interpreted as a
;;;; num-lock modifier.
(defun compute-num-lock-mask (display)
  ;; Loop over the modifier positions corresponding to Mod1
  ;; through Mod5.
  (loop with mask = #b00000000
	with keysym = (clim-xcommon:keysym-name-to-keysym :NUM-LOCK)
	for index from 3 to 7
	for keycodes = (nth-value index (xlib:modifier-mapping display))
	do (loop for keycode in keycodes
		 when (= keysym (xlib:keycode->keysym display keycode 0))
		   do (setf mask (logior mask (ash 1 index))))
	finally (return mask)))

;;;; Whether a caps-lock or a shift-lock modifier is in effect is a
;;;; bit trickier, because there is only one bit position in the
;;;; modifier mask corresponding to both these modifiers.  The rules
;;;; described below make it possible for that mask bit to mean either
;;;; or both of those modifiers.  If it can mean both, then caps-lock
;;;; takes precedence.

;;;; CAPS-LOCK: Whether caps-lock is in effect is controlled by the
;;;; lock bit position in the modifier mask in effect when a key-press
;;;; event occurs.  Whether caps-lock is in effect is controlled by a
;;;; two things: an assignment of some keycode K to the keysym named
;;;; :CAPS-LOCK, and the attachment of that same keycode K to the lock
;;;; modifier.  We can determine whether this is the case by looking
;;;; at the return value of a call to XLIB:MODIFIER-MAPPING
;;;; corresponding to the lock modifier, and then call
;;;; XLIB:KEYCODE->KEYSYM with each of the keycodes in that return
;;;; value to check whether it is assigned to the keysym named
;;;; :CAPS-LOCK.  As before, this information needs to be determined
;;;; only at start-up and when the keyboard mapping changes.  We
;;;; summarize this information as a mask that has a 1 in the position
;;;; corresponding to the lock modifier the lock modifier should be
;;;; interpreted as a caps-lock modifier.
(defun compute-caps-lock-mask (display)
  (let* ((keysym (clim-xcommon:keysym-name-to-keysym :CAPS-LOCK))
	 (keycodes  (nth-value 1 (xlib:modifier-mapping display))))
    (loop for keycode in keycodes
	  when (= keysym (xlib:keycode->keysym display keycode 0))
	    return #b00000010
	finally (return #b00000000))))

;;;; SHIFT-LOCK: Whether shift-lock is in effect is controlled by the
;;;; lock bit position in the modifier mask in effect when a key-press
;;;; event occurs, in exactly the same way as caps-lock is controlled.
;;;; Therefore, only one of the two can be in effect.  Whether
;;;; shift-lock is in effect is determined in exactly the same way as
;;;; for caps-lock with the difference that some keycode must be
;;;; assigned to the keysym named :SHIFT-LOCK.  The mask for to the
;;;; shift-lock information contains a 1 in the bit position
;;;; corresponding to the lock modifier only if the mask for the
;;;; caps-lock information does not contain a 1 in that position.
;;;; This way, if a lock modifier can be interpreted as wither
;;;; shift-lock or caps-lock, then caps-lock take precedence.
(defun compute-shift-lock-mask (display)
  (if (not (zerop (compute-caps-lock-mask display)))
      #b00000000
      (let* ((keysym (clim-xcommon:keysym-name-to-keysym :SHIFT-LOCK))
	     (keycodes  (nth-value 1 (xlib:modifier-mapping display))))
	(loop for keycode in keycodes
	      when (= keysym (xlib:keycode->keysym display keycode 0))
		return #b00000010
	      finally (return #b00000000)))))

;;;; There are two steps involved in this process.
;;;;
;;;; The first step is executed when a CLX port is created, and when
;;;; the keyboard mapping of a port is altered.  In this step, we
;;;; create a KEYSYM-INTERPRETATION instance that determines how a
;;;; modifier mask should be interpreted.  The interpretation of the
;;;; modifier mask depends on the assignment of keycodes to certain
;;;; keysyms, and of the attachment of those keycodes to modifiers.

(defclass keysym-interpretation ()
  (;; This slot contains a mask to be applied to an X11 modifier mask
   ;; to determine whether the "mode switch" modifier is in effect.
   (%mode-switch-mask :initarg :mode-switch-mask
		      :initform #b00000000
		      :accessor mode-switch-mask)
   (%num-lock-mask :initarg :num-lock-mask
		   :initform #b00000000
		   :accessor num-lock-mask)
   (%shift-lock-mask :initarg :shift-lock-mask
		     :initform #b00000000
		     :accessor shift-lock-mask)
   (%caps-lock-mask :initarg :caps-lock-mask
		    :initform #b00000000
		    :accessor caps-lock-mask)))

(defun compute-keysym-interpretation (display)
  (make-instance 'keysym-interpretation
    :mode-switch-mask (compute-mode-switch-mask display)
    :num-lock-mask (compute-num-lock-mask display)
    :shift-lock-mask (compute-shift-lock-mask display)
    :caps-lock-mask (compute-caps-lock-mask display)))

;;; Return true if and only if the mode switch is in effect.  This is
;;; the case when the modifier mask contains a 1 in a position that
;;; has been assigned to the mode switch modifier.
(defun mode-switch-in-effect-p (keysym-interpretation modifier-mask)
  (plusp (logand (mode-switch-mask keysym-interpretation) modifier-mask)))

;;; Return true if and only if numeric lock is in effect.  This is the
;;; case when the modifier mask contains a 1 in a position that has
;;; been assigned to the num-lock modifier.
(defun num-lock-in-effect-p (keysym-interpretation modifier-mask)
  (plusp (logand (num-lock-mask keysym-interpretation) modifier-mask)))

;;; Return true if and only if the lock modifier bit is on.  This is
;;; the case when the modifier mask contains a 1 in a position 1.
(defun lock-in-effect-p (modifier-mask)
  (logbitp 1 modifier-mask))

;;; Return true if and only if shift-lock is in effect.  This is the
;;; case when the modifier mask contains a 1 in a position that has
;;; been assigned to the shift-lock modifier.
(defun shift-lock-in-effect-p (keysym-interpretation modifier-mask)
  (plusp (logand (shift-lock-mask keysym-interpretation) modifier-mask)))

;;; Return true if and only if the shift modifier bit is on.  This is
;;; the case when the modifier mask contains a 1 in a position 0.
(defun shift-in-effect-p (modifier-mask)
  (logbitp 0 modifier-mask))

;;; Return true if and only if caps-lock is in effect.  This is the
;;; case when the modifier mask contains a 1 in a position that has
;;; been assigned to the caps-lock modifier.
(defun caps-lock-in-effect-p (keysym-interpretation modifier-mask)
  (plusp (logand (caps-lock-mask keysym-interpretation) modifier-mask)))

(defparameter *keypad-keysym-names*
  '(:KP-SPACE :KP-TAB :KP-ENTER :KP-F1 :KP-F2 :KP-F3 :KP-F4
    :KP-HOME :KP-LEFT :KP-UP :KP-RIGHT :KP-DOWN
    :KP-PRIOR :KP-PAGE-UP :KP-NEXT :KP-PAGE-DOWN
    :KP-END :KP-BEGIN :KP-INSERT :KP-DELETE
    :KP-EQUAL :KP-MULTIPLY :KP-ADD :KP-SEPARATOR
    :KP-SUBTRACT[ :KP-DECIMAL :KP-DIVIDE :KP-0
    :KP-1 :KP-2 :KP-3 :KP-4 :KP-5 :KP-6 :KP-7 :KP-8 :KP-9))

(defparameter *keypad-table*
  (let ((result (make-hash-table :test #'eql)))
    (loop for keysym-name in *keypad-keysym-names*
	  for keysym = (clim-xcommon:keysym-name-to-keysym keysym-name)
	  do (setf (gethash keysym result) t))
    result))

(defun keypad-keysym-p (keysym)
  (gethash keysym *keypad-table*))

;;; This hash table maps each keysym that is considered lower-case
;;; alphabetic to its upper-case counterpart.
(defparameter *lower-case-alphabetic-table*
  (let ((result (make-hash-table :test #'eql)))
    ;; ASCII lower-case characters.
    (loop for keysym from #x61 to #x7a
	  do (setf (gethash keysym result) (- keysym #x20)))
    ;; More Latin-1 characters.
    (loop for keysym from #xe0 to #xf6
	  do (setf (gethash keysym result) (- keysym #x20)))
    (loop for keysym from #xf8 to #xff
	  do (setf (gethash keysym result) (- keysym #x20)))
    ;; Latin-2 characters.
    (loop for keysym in '(#x1b1 #x1b3 #x1b5 #x1b6 #x1b9
			  #x1ba #x1bb #x1bc #x1be #x1bf)
	  do (setf (gethash keysym result) (- keysym #x10)))
    (loop for keysym in '(#x1e0 #x1e3 #x1e5 #x1e6 #x1e8 #x1ea #x1ec #x1ef
			  #x1f0 #x1f1 #x1f2 #x1f5 #x1fb #x1f8 #x1f9 #x1f0e)
	  do (setf (gethash keysym result) (- keysym #x20)))
    ;; Latin-3 characters.
    (loop for keysym in '(#x3b3 #x3b5 #x3b6 #x3ba #x3bb #x3bc)
	  do (setf (gethash keysym result) (- keysym #x10)))
    (loop for keysym in '(#x3e0 #x3e7 #x3ec #x3ef #x3e0
			  #x3f1 #x3f2 #x3f3 #x3f9 #x3fd #x3fe)
	  do (setf (gethash keysym result) (- keysym #x20)))
    ;; Latin-9 characters.
    (setf (gethash #x13bd result) #x13bc)
    ;; FIXME, do more characters
    result))

(defun lower-case-keysym-p (keysym)
  (gethash keysym *lower-case-alphabetic-table*))

;;; Rule 1 applies when the num-lock modifier is on in MODIFIER-MASK
;;; and the second keysym in the group is a keypad keysym.  Which
;;; group to use is indicated by OFFSET which is 0 if group 1 is to be
;;; used and 2 if group 2 is to be used.
(defun rule-1 (display keysym-interpretation keycode modifier-mask offset)
  (if (and (num-lock-in-effect-p keysym-interpretation modifier-mask)
	   (keypad-keysym-p (xlib:keycode->keysym display keycode (1+ offset))))
      ;; Rule 1 applies.
      (xlib:keycode->keysym
       display
       keycode
       (if (or (shift-in-effect-p modifier-mask)
	       (shift-lock-in-effect-p keysym-interpretation modifier-mask))
	   offset
	   (1+ offset)))
      ;; Rule 1 does not apply.  Return false.
      nil))

;;; Rule 2 applies when the SHIFT and LOCK modifiers are both off.  It
;;; says that if both the SHIFT and the LOCK modifier bits are off,
;;; then the first keysym in the group is returned.
(defun rule-2 (display keycode modifier-mask offset)
  (if (not (or (shift-in-effect-p modifier-mask)
	       (lock-in-effect-p modifier-mask)))
      ;; Rule 2 applies.
      (xlib:keycode->keysym
       display
       keycode
       offset)
      ;; Rule 2 does not apply.  Return false.
      nil))

;;; Rule 3 applies when the shift modifier is off and the caps-lock
;;; modifier is on.
(defun rule-3 (display keysym-interpretation keycode modifier-mask offset)
  (if (and (not (shift-in-effect-p modifier-mask))
	   (caps-lock-in-effect-p keysym-interpretation modifier-mask))
      (let* ((first (xlib:keycode->keysym display keycode offset))
	     (upper (lower-case-keysym-p first)))
	(if (null upper) first upper))
      ;; Rule 3 does not apply.  Return false.
      nil))

;;; Rule 4 applies when the shift modifier and the caps-lock modifier
;;; are both on.
(defun rule-4 (display keysym-interpretation keycode modifier-mask offset)
  (if (and (shift-in-effect-p modifier-mask)
	   (caps-lock-in-effect-p keysym-interpretation modifier-mask))
      ;; Rule 4 applies.
      (let* ((second (xlib:keycode->keysym display keycode (1+ offset)))
	     (upper (lower-case-keysym-p second)))
	(if (null upper) second upper))
      ;; Rule 4 does not apply.  Return false.
      nil))

;;; Rule 5 applies when the shift modifier and the shift-lock modifier
;;; are both on.
(defun rule-5 (display keysym-interpretation keycode modifier-mask offset)
  (if (and (shift-in-effect-p modifier-mask)
	   (shift-lock-in-effect-p keysym-interpretation modifier-mask))
      ;; Rule 5 applies.
      (xlib:keycode->keysym display keycode (1+ offset))
      ;; Rule 5 does not apply.  Return false.
      nil))

(defun keycode-to-keysym (display keysym-interpretation keycode modifier-mask)
  (let ((offset (if (mode-switch-in-effect-p keysym-interpretation modifier-mask)
		    0
		    2)))
    (or (rule-1 display keysym-interpretation keycode modifier-mask offset)
	(rule-2 display keycode modifier-mask offset)
	(rule-3 display keysym-interpretation keycode modifier-mask offset)
	(rule-4 display keysym-interpretation keycode modifier-mask offset)
	(rule-5 display keysym-interpretation keycode modifier-mask offset))))
