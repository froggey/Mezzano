;;; -*- Mode: Lisp; Package: CLIM-CLX; -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000,2001 by
;;;           Iban Hatchondo (hatchond@emi.u-bordeaux.fr)
;;;           Julien Boninfante (boninfan@emi.u-bordeaux.fr)
;;;  (c) copyright 2000, 2001, 2014, 2016 by
;;;           Robert Strandh (robert.strandh@gmail.com)

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

;;; Think about rewriting this macro to be nicer.
(defmacro peek-event ((display &rest keys) &body body)
  (let ((escape (gensym)))
    `(block ,escape
       (xlib:process-event ,display :timeout 0 :peek-p t :handler
         #'(lambda (&key ,@keys &allow-other-keys)
             (return-from ,escape
               (progn
                 ,@body)))))))

(defun decode-x-button-code (code)
  (let ((button-mapping #.(vector +pointer-left-button+
                                  +pointer-middle-button+
                                  +pointer-right-button+
                                  +pointer-wheel-up+
                                  +pointer-wheel-down+
                                  +pointer-wheel-left+
                                  +pointer-wheel-right+))
        (code (1- code)))
    (when (and (>= code 0)
               (< code (length button-mapping)))
      (aref button-mapping code))))

;;; From "Inter-Client Communication Conventions Manual", Version
;;; 2.0.xf86.1, section 4.1.5:
;;;
;;; |   Advice to Implementors
;;; |
;;; |   Clients cannot distinguish between the case where a top-level
;;; |   window is resized and moved from the case where the window is
;;; |   resized but not moved, since a real ConfigureNotify event will be
;;; |   received in both cases. Clients that are concerned with keeping
;;; |   track of the absolute position of a top-level window should keep
;;; |   a piece of state indicating whether they are certain of its
;;; |   position. Upon receipt of a real ConfigureNotify event on the
;;; |   top-level window, the client should note that the position is
;;; |   unknown. Upon receipt of a synthetic ConfigureNotify event, the
;;; |   client should note the position as known, using the position in
;;; |   this event. If the client receives a KeyPress, KeyRelease,
;;; |   ButtonPress, ButtonRelease, MotionNotify, EnterNotify, or
;;; |   LeaveNotify event on the window (or on any descendant), the
;;; |   client can deduce the top-level window's position from the
;;; |   difference between the (event-x, event-y) and (root-x, root-y)
;;; |   coordinates in these events. Only when the position is unknown
;;; |   does the client need to use the TranslateCoordinates request to
;;; |   find the position of a top-level window.

;;; The moral is that we need to distinguish between synthetic and
;;; genuine configure-notify events. We expect that synthetic
;;; configure notify events come from the window manager and state the
;;; correct size and position, while genuine configure events only
;;; state the correct size.

;;; NOTE: Although it might be tempting to compress (consolidate)
;;; events here, this is the wrong place. In our current architecture
;;; the process calling this function (the port's event handler
;;; process) just reads the events from the X server, and does it with
;;; almost no lack behind the reality. While the application frame's
;;; event top level loop does the actual processing of events and thus
;;; may produce lack. So the events have to be compressed in the
;;; frame's event queue.
;;;
;;; So event compression is implemented in EVENT-QUEUE-APPEND.
;;;
;;; This changes for possible _real_ immediate repainting sheets, here
;;; a possible solution for the port's event handler loop can be to
;;; read all available events off into a temponary queue (and event
;;; compression for immediate events is done there) and then dispatch
;;; all events from there as usual.
;;;
;;;--GB

;;; XXX :button code -> :button (decode-x-button-code code)
;;;
;;; Only button and keypress events get a :code keyword argument! For
;;; mouse button events, one should use decode-x-button-code;
;;; otherwise one needs to look at the state argument to get the
;;; current button state. The CLIM spec says that pointer motion
;;; events are a subclass of pointer-event, which is reasonable, but
;;; unfortunately they use the same button slot, whose value should
;;; only be a single button. Yet pointer-button-state can return the
;;; logical or of the button values... aaargh. For now I'll
;;; canonicalize the value going into the button slot and think about
;;; adding a pointer-event-buttons slot to pointer events. -- moore

(defvar *clx-port*)
(defvar *wait-function*)

(defun event-handler (&key display window kind event-key code state mode time
                        type width height x y root-x root-y
                        data override-redirect-p send-event-p
                        target property requestor selection
                        request first-keycode count
                        &allow-other-keys)
  (declare (ignore first-keycode count))
  (when (eql event-key :mapping-notify)
    (xlib:mapping-notify display request 0 0)
    (return-from event-handler (maybe-funcall *wait-function*)))
  (when-let ((sheet (and window (port-lookup-sheet *clx-port* window))))
    (case event-key
      ((:key-press :key-release)
       (multiple-value-bind (keyname modifier-state keysym-name)
           (x-event-to-key-name-and-modifiers *clx-port*
                                              event-key code state)
         (make-instance (if (eq event-key :key-press)
                            'key-press-event
                            'key-release-event)
                        :key-name keysym-name
                        :key-character (and (characterp keyname) keyname)
                        :x x :y y
                        :graft-x root-x
                        :graft-y root-y
                        :sheet (or (frame-properties (pane-frame sheet) 'focus) sheet)
                        :modifier-state modifier-state :timestamp time)))
      ((:button-press :button-release)
       (let ((modifier-state (clim-xcommon:x-event-state-modifiers *clx-port* state))
             (button (decode-x-button-code code)))
         (if (member button '(#.+pointer-wheel-up+
                              #.+pointer-wheel-down+
                              #.+pointer-wheel-left+
                              #.+pointer-wheel-right+))
             ;; Pointer scroll generates button press and button
             ;; release event. We ignore the latter. -- jd 2019-09-01
             (when (eq event-key :button-press)
               (make-instance 'climi::pointer-scroll-event
                              :pointer 0
                              :button button :x x :y y
                              :graft-x root-x
                              :graft-y root-y
                              :sheet sheet
                              :modifier-state modifier-state
                              :delta-x (case button
                                         (#.+pointer-wheel-left+ -1)
                                         (#.+pointer-wheel-right+ 1)
                                         (otherwise 0))
                              :delta-y (case button
                                         (#.+pointer-wheel-up+ -1)
                                         (#.+pointer-wheel-down+ 1)
                                         (otherwise 0))
                              :timestamp time))
             (make-instance (if (eq event-key :button-press)
                                'pointer-button-press-event
                                'pointer-button-release-event)
                            :pointer 0
                            :button button :x x :y y
                            :graft-x root-x
                            :graft-y root-y
                            :sheet sheet :modifier-state modifier-state
                            :timestamp time))))
      ((:leave-notify :enter-notify)
       ;; Ignore :{ENTER,LEAVE}-NOTIFY events of kind :INFERIOR unless
       ;; the mode is :[UN]GRAB.
       ;;
       ;; The :INFERIOR kind corresponds to the pointer moving from a
       ;; parent window to a child window which we do not consider
       ;; leaving the parent.
       ;;
       ;; But we cannot ignore any :[UN]GRAB events since doing so
       ;; would violate the stack-properties of enter/exit event
       ;; sequences.
       ;;
       ;; The event kinds filtered here must be coordinated with the
       ;; processing in the DISTRIBUTE-EVENTS method for BASIC-PORT
       ;; and related methods.
       (when (or (not (eq kind :inferior))
                 (member mode '(:grab :ungrab)))
         (make-instance (case event-key
                          (:leave-notify (case mode
                                           (:grab 'pointer-grab-leave-event)
                                           (:ungrab 'pointer-ungrab-leave-event)
                                           (t 'pointer-exit-event)))
                          (:enter-notify (case mode
                                           (:grab 'pointer-grab-enter-event)
                                           (:ungrab 'pointer-ungrab-enter-event)
                                           (t 'pointer-enter-event))))
                        :pointer 0 :button code
                        :x x :y y
                        :graft-x root-x
                        :graft-y root-y
                        :sheet sheet
                        :modifier-state (clim-xcommon:x-event-state-modifiers
                                         *clx-port* state)
                        :timestamp time)))
      (:configure-notify
       (cond ((and (eq (sheet-parent sheet) (graft sheet))
                   (graft sheet)
                   (not override-redirect-p)
                   (not send-event-p))
              ;; Genuine top-level-sheet event (with override-redirect off).
              ;;
              ;; Since the root window is not our real parent, but there the
              ;; window managers decoration in between, only the size is
              ;; correct, so we need to query coordinates from the X
              ;; server. Note that sheet relative coodinates may be something
              ;; different than [0,0].
              (multiple-value-bind (x y)
                  (xlib:translate-coordinates window
                                              0
                                              0
                                              (clx-port-window *clx-port*))
                (make-instance 'window-configuration-event
                               :sheet sheet
                               :x x
                               :y y
                               :width width :height height)))
             (t
              ;; nothing special here
              (make-instance 'window-configuration-event
                             :sheet sheet
                             :x x :y y :width width :height height))))
      (:destroy-notify
       (make-instance 'window-destroy-event :sheet sheet))
      (:motion-notify
       (let ((modifier-state (clim-xcommon:x-event-state-modifiers *clx-port*
                                                                   state)))
         (make-instance 'pointer-motion-event
                        :pointer 0 :button code
                        :x x :y y
                        :graft-x root-x
                        :graft-y root-y
                        :sheet sheet
                        :modifier-state modifier-state
                        :timestamp time)))
      ((:exposure :display :graphics-exposure)
       ;; Notes:
       ;; . Do not compare count with 0 here, last rectangle in an
       ;;   :exposure event sequence does not cover the whole region.
       ;;
       ;; . Do not transform the event region here, since
       ;;   WINDOW-EVENT-REGION does it already. And rightfully so.
       ;;   (think about changing a sheet's native transformation).
       ;;--GB
       ;;
       ;; Mike says:
       ;;   One of the lisps is bogusly sending a :display event instead of an
       ;; :exposure event. I don't remember if it's CMUCL or SBCL. So the
       ;; :display event should be left in.
       ;;
       (make-instance 'window-repaint-event
                      :timestamp time
                      :sheet sheet
                      :region (make-rectangle* x y (+ x width) (+ y height))))
      ;; port processes selection events synchronously and there is
      ;; no event passed to the rest of the system.
      (:selection-notify
       (process-selection-notify *clx-port* window target property selection time)
       (maybe-funcall *wait-function*))
      (:selection-clear
       (process-selection-clear *clx-port* selection)
       (maybe-funcall *wait-function*))
      (:selection-request
       (process-selection-request *clx-port* window sheet target property requestor selection time)
       (maybe-funcall *wait-function*))
      (:client-message
       (or (port-client-message sheet time type data)
           (maybe-funcall *wait-function*)))
      (t
       (unless (xlib:event-listen (clx-port-display *clx-port*))
         (xlib:display-force-output (clx-port-display *clx-port*)))
       (maybe-funcall *wait-function*)))))


;; Handling of X client messages

;;; this client message is only necessary if we advertise that we
;;; participate in the :WM_TAKE_FOCUS protocol; otherwise, the window
;;; manager is responsible for all setting of input focus for us.  If
;;; we want to do something more complicated with server input focus,
;;; then this method should be adjusted appropriately and the
;;; top-level-sheet REALIZE-MIRROR method should be adjusted to add
;;; :WM_TAKE_FOCUS to XLIB:WM-PROTOCOLS.  CSR, 2009-02-18

;;; And that's what we do. top-level-sheet maintains last focused
;;; sheet among its children and upon :WM_TAKE_FOCUS it assigns back
;;; the focus to it. Currently we have implemented click-to-focus
;;; policy which is enforced in basic-port's distribute-event
;;; method. -- jd 2019-08-26

(defun port-client-message (sheet time type data)
  (case type
    (:wm_protocols
     (let ((message (xlib:atom-name (slot-value *clx-port* 'display) (aref data 0))))
       (case message
         (:wm_take_focus
          ;; hmm, this message seems to be sent twice.
          (when-let ((mirror (sheet-xmirror sheet)))
            (xlib:set-input-focus (clx-port-display *clx-port*)
                                  mirror :parent (elt data 1)))
          (make-instance 'window-manager-focus-event :sheet sheet :timestamp time))
         (:wm_delete_window
          (make-instance 'window-manager-delete-event :sheet sheet :timestamp time))
         (otherwise
          (warn "Unprocessed WM Protocols message: ~:_message = ~S;~:_ data = ~S;~_ sheet = ~S."
                message data sheet)))))
    (otherwise
     (warn "Unprocessed client message: ~:_type = ~S;~:_ data = ~S;~_ sheet = ~S."
           type data sheet))))

(defmethod process-next-event ((port clx-basic-port) &key wait-function (timeout nil))
  (let ((*clx-port* port)
        (*wait-function* wait-function))
    (when (maybe-funcall wait-function)
      (return-from process-next-event
        (values nil :wait-function)))
    (let ((event (xlib:process-event (clx-port-display port)
                                     :timeout timeout
				     :handler #'event-handler
                                     :discard-p t
                                     :force-output-p t)))
      (case event
        ((nil)
         (if (maybe-funcall wait-function)
             (values nil :wait-function)
             (values nil :timeout)))
        ((t)
         (values nil :wait-function))
        (otherwise
         (prog1 t
           (distribute-event port event)))))))

;;; pointer button bits in the state mask

;;; Happily, The McCLIM pointer constants correspond directly to the X
;;; constants.

(defconstant +right-button-mask+ #x100)
(defconstant +middle-button-mask+ #x200)
(defconstant +left-button-mask+ #x400)
(defconstant +wheel-up-mask+ #x800)
(defconstant +wheel-down-mask+ #x1000)

(defmethod pointer-button-state ((pointer clx-basic-pointer))
  (multiple-value-bind (x y same-screen-p child mask)
      (xlib:query-pointer (clx-port-window (port pointer)))
    (declare (ignore x y same-screen-p child))
    (ldb (byte 5 8) mask)))

(defun button-from-state (mask)
  ;; In button events we don't want to see more than one button,
  ;; according to the spec, so pick a canonical ordering. :P The mask
  ;; is that state mask from an X event.
  (cond ((logtest +right-button-mask+ mask)
         +pointer-right-button+)
        ((logtest +middle-button-mask+ mask)
         +pointer-middle-button+)
        ((logtest +left-button-mask+ mask)
         +pointer-left-button+)
        ((logtest +wheel-up-mask+ mask)
         +pointer-wheel-up+)
        ((logtest +wheel-down-mask+ mask)
         +pointer-wheel-down+)
        (t 0)))

(defmethod port-modifier-state ((port clx-basic-port))
  (multiple-value-bind (x y same-screen-p child mask)
      (xlib:query-pointer (clx-port-window port))
    (declare (ignore x y same-screen-p child))
    (clim-xcommon:x-event-state-modifiers port mask)))

(defmethod synthesize-pointer-motion-event ((pointer clx-basic-pointer))
  (when-let* ((port (port pointer))
              ;; XXX Should we rely on port-pointer-sheet being correct? -- moore
              (sheet (port-pointer-sheet port))
              (ancestor (sheet-mirrored-ancestor sheet))
              (mirror (sheet-direct-xmirror ancestor)))
    (multiple-value-bind (x y same-screen-p child mask root-x root-y)
        (xlib:query-pointer mirror)
      (declare (ignore child))
      (when same-screen-p
        (multiple-value-bind (x y)
            (if (eq sheet ancestor)
                (values x y)
                (untransform-position (sheet-native-transformation sheet) x y))
          (make-instance 'pointer-motion-event
                         :pointer 0 :button (button-from-state mask)
                         :x x :y y :graft-x root-x :graft-y root-y
                         :sheet sheet
                         :modifier-state (clim-xcommon:x-event-state-modifiers
                                          port mask)))))))

(defmethod port-frame-keyboard-input-focus ((port clx-basic-port) frame)
  (frame-properties frame 'focus))
(defmethod (setf port-frame-keyboard-input-focus) (focus (port clx-basic-port) frame)
  (setf (frame-properties frame 'focus) focus))

(defmethod port-grab-pointer ((port clx-basic-port) pointer sheet)
  (let ((mirror (sheet-xmirror sheet))
        (events '(:button-press :button-release
	          :leave-window :enter-window
	          :pointer-motion)))
    ;; Probably we want to set :cursor here..
    (eq :success (xlib:grab-pointer mirror events :owner-p t))))

(defmethod port-ungrab-pointer ((port clx-basic-port) pointer sheet)
  (declare (ignore pointer sheet))
  (xlib:ungrab-pointer (clx-port-display port)))

;;; Modifier cache support

;;; Recall that XLIB:MODIFIER-MAPPING returns 8 values.  Each value is
;;; a list of keycodes (in some arbitrary order) that are currently
;;; used to mean a particular modifier.  Each value as the following
;;; meaning:
;;;
;;;   value number  meaning
;;;        0        shift keycodes
;;;        1        lock keycodes
;;;        2        control keycodes
;;;        3        mod1 keycodes
;;;        4        mod2 keycodes
;;;        5        mod3 keycodes
;;;        6        mod4 keycodes
;;;        7        mod5 keycodes
;;;
;;; The problem here is that a keycode can be a member of more than
;;; one list.  For example, if you turn your caps lock key into an
;;; additional control key, then the keycode for the caps lock key may
;;; very well be a member both of the list in value 1 and the list in
;;; value 2.
;;;
;;; Let us take the case of caps lock.  The X11 programming manual
;;; tells us that lock modifier is interpreted as caps lock when the
;;; keysym named :CAPS-LOCK (as used by CLX) is attached to some
;;; keycode and that keycode is also attached (as determined by
;;; XLIB:MODIFIER-MAPPING) to the lock modifier, i.e., that keycode is
;;; a member of the list in value 1.  The converse seems to be untrue,
;;; though.  Just because someone pressed a key that satisfies those
;;; criteria does not mean that the X11 server will switch on the lock
;;; modifier next time a key is pressed.  It is unclear what the
;;; criteria the X11 server uses.  But for our purpose it is important
;;; to start by checking the lock modifier first.

;;; This method creates a modifier mapping that is similar in spirit
;;; to the one returned by XLIB:MODIFIER-MAPPING.  It differs from the
;;; XLIB function in two ways.  First it returns a vector of length 8
;;; rather than 8 different values.  Second, each element of the
;;; vector is a list of keysym names rather than of keycodes.  Recall
;;; that a keysym name is a Common Lisp symbol in the KEYWORD package.
(defmethod clim-xcommon:modifier-mapping ((port clx-basic-port))
  (let* ((display (clx-port-display port))
	 (x-modifiers (multiple-value-list (xlib:modifier-mapping display)))
	 (modifier-map (make-array (length x-modifiers) :initial-element nil)))
    (loop
       for keycodes in x-modifiers
       for i from 0
       do (setf (aref modifier-map i)
		(mapcan (lambda (keycode)
			  (modifier-keycode->keysyms display keycode))
			keycodes)))
    modifier-map))
