;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLIM-CLX; -*-
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

(in-package :clim-clx)

;;; Recall that the function CLIM-XCOMMON:KEYSYM-TO-KEYSYM-NAME simply
;;; consults a fixed hash table that maps X11 keysyms (which are
;;; numbers) to keysym names (which are Common Lisp symbols in the
;;; KEYWORD package).
;;;
;;; This function returns a list of length 0, 1 or 2.  It returns the
;;; empty list if the keysym with index 0 is 0.  I don't see how this
;;; can be the case, though.  Otherwise, it returns a singleton list
;;; if the keysyms with index 0 and 1 are the same, and a list of the
;;; keysyms with index 0 and 1 if the two are different.
;;;
;;; I am guessing that for all modifier keys, the two are the same, so
;;; that this function always returns a singleton list.
(defun modifier-keycode->keysyms (display keycode)
  (let ((first-x-keysym (xlib:keycode->keysym display keycode 0)))
    (when (zerop first-x-keysym)
      (return-from modifier-keycode->keysyms nil))
    (let ((second-x-keysym (xlib:keycode->keysym display keycode 1)))
      (cons (clim-xcommon:keysym-to-keysym-name first-x-keysym)
	    (if (eql first-x-keysym second-x-keysym)
		'()
		(list (clim-xcommon:keysym-to-keysym-name second-x-keysym)))))))

;;; The X state is the state before the current event, so key events
;;; for the modifier keys don't reflect the state that results from
;;; pressing or releasing those keys.  We want the CLIM modifiers to
;;; reflect the post event state.

;;; Recall that X11 defines how to map keycodes to keysyms by defining
;;; a "list" (not a Common Lisp list, though.  More like a vector in
;;; fact.) of possible keysyms for each keycode.  The third argument
;;; to XLIB:KEYCODE->KEYSYM is an index into that list.  The standard
;;; rules make use only of indices 0 to 3 in that list.  Indices 0 and
;;; 1 are considered members of "Group 1" and indices 2 and 3 are
;;; members of "Group 2".
;;;
;;; The Xlib C language library function XKeycodeToKeysym might return
;;; some value corresponding NoSymbol for certain values of the index,
;;; in particular for index 1 when the keycode corresponds to an
;;; alphabetic symbol with both a lower and an upper case version, CLX
;;; applies the rules for us, so that in that case, index 1 is the
;;; keysym of the upper-case version of the character.
;;;
;;; The parameter STATE is a bit mask represented as the logical OR
;;; of individual bits.  Each bit corresponds to a modifier or a
;;; pointer button that is active immediately before the key was
;;; pressed or released.  The bits have the following meaning:
;;;
;;;   position  value    meaning
;;;     0         1      shift
;;;     1         2      lock
;;;     2         4      control
;;;     3         8      mod1
;;;     4        16      mod2
;;;     5        32      mod3
;;;     6        64      mod4
;;;     7       128      mod5
;;;     8       256      button1
;;;     9       512      button2
;;;    10      1024      button3
;;;    11      2048      button4
;;;    12      4096      button5

(defun x-event-to-key-name-and-modifiers (port event-key keycode state)
  (multiple-value-bind (clim-modifiers caps-lock? mode-switch?)
      (clim-xcommon:x-event-state-modifiers port state)
    (let* ((display (clx-port-display port))
           (shift? (logtest +shift-key+ clim-modifiers))
           (shifted-keysym (xlib:keycode->keysym display keycode
                                                 (+ 1 (if mode-switch?
                                                          2 0))))
           (unshifted-keysym (xlib:keycode->keysym display keycode
                                                   (if mode-switch?
                                                       2 0)))
           (keysym-char (xlib:keysym->character display unshifted-keysym
                                                (if mode-switch? 2 0)))
           (alpha-char? (and (characterp keysym-char)
                             (alpha-char-p keysym-char)))
           (keysym
            (if shift?
                ;; Shift + caps lock cancel themselves for alphabetic chars
                (if (and caps-lock? alpha-char?)
                    unshifted-keysym
                    shifted-keysym)
                (if (and caps-lock? alpha-char?)
                    shifted-keysym
                    unshifted-keysym))))
      (let* ((keysym-name (clim-xcommon:keysym-to-keysym-name keysym))
             (char (xlib:keysym->character display keysym
                                           (+ (if shift?
                                                  1 0)
                                              (if mode-switch?
                                                  2 0))))
             ;; Cache might be updated at this step.
             (modifiers (clim-xcommon:x-keysym-to-clim-modifiers
                         port
                         event-key
                         char
                         (clim-xcommon:keysym-to-keysym-name keysym)
                         state)))
        (values char
                ;; We filter away the shift state if there is a
                ;; difference between the shifted and unshifted
                ;; keysym. This is so eg. #\A will not look like "#\A
                ;; with a Shift modifier", as this makes gesture
                ;; processing more difficult.
                (if (= shifted-keysym unshifted-keysym)
                    modifiers
                    (logandc2 modifiers +shift-key+))
                keysym-name)))))

;;;;

(defun numeric-keysym-to-character (keysym)
  (and (<= 0 keysym 255)
       (code-char keysym)))

(defun keysym-to-character (keysym)
  (numeric-keysym-to-character (clim-xcommon:keysym-name-to-keysym keysym)))
