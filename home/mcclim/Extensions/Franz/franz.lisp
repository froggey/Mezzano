(in-package :climi)

(defun dragging-drawing (stream drawer &key (finish-on-release t)
                         (pointer (port-pointer (port stream)))
                         multiple-window)
  "Draws something simple in response to pointer events for
`pointer' and returns the coordinates of the pointer when the
function finishes. The function finishes when mouse button one is
no longer held down if `finish-on-release' is true; if it is
false, it finishes when the mouse is clicked. `Drawer' should
draw something on `stream', and is called with tree arguments:
two integers, the X and the Y coordinates for the pointer motion
triggering the draw, and either the symbol `:draw' or `:erase'
signalling what the function should do. `Drawer' will be called
with the previously used coordinates whenever pointer motion
occurs, so it can erase the previous output (elegantly done by
using `+flipping-ink+' for drawing and ignoring the state
symbol)."
  (with-output-recording-options (stream :draw t :record nil)
    (let ((ox nil) (oy nil))           ; So we can erase the old line.
      (labels ((end (x y)
                 (when ox (funcall drawer ox oy :draw))
                 (return-from dragging-drawing
                   (values x y))))
        (tracking-pointer (stream :pointer pointer
                                  :multiple-window multiple-window)
          (:pointer-motion (x y)
            (when ox (funcall drawer x y :erase))
            (funcall drawer x y :draw)
            (setf ox x oy y))
          (:pointer-button-press (x y)
            (end x y))
          (:pointer-button-release (x y)
            (when finish-on-release
              (end x y))))))))

(defun clime:pointer-place-rubber-band-line* (&key (stream *standard-output*)
                                        (pointer (port-pointer (port stream)))
                                        multiple-window start-x start-y
                                        (finish-on-release t))
  "Let the user drag a line on `stream', returning the
coordinates of the line ends as four values. `Pointer' is the
pointer that will be tracked (the default should be used unless
the port has multiple pointing devices), `multiple-window' is
currently unimplemented and `start-x'/`start-y', if provided (and
both or none must be provided) are the coordinates for one end of
the line. If these arguments are not provided, the user will have
to press a mouse button to specify the beginning of the line. If
`finish-on-release' is true, the function will end when the user
releases the mouse button. If false, the user will have to click
to finish inputting the line."
  (assert (not (eq (not (not start-x)) (not start-y))) nil
          "You must provide either both `:start-x' and `:start-y'
or none at all")
  (or start-x
      (block nil
        (tracking-pointer (stream :pointer pointer
                                  :multiple-window multiple-window)
          (:pointer-button-press (event x y)
                                 (declare (ignore event))
                                 (setf start-x x)
                                 (setf start-y y)
                                 (return)))))
  (assert (and (>= start-x 0) (>= start-y 0)))
  (labels ((draw (x y state)
             (declare (ignore state))
             (with-drawing-options (stream :ink +flipping-ink+)
               (draw-line* stream start-x start-y x y))))
    (multiple-value-call #'values
      (values start-x start-y)
      (dragging-drawing stream #'draw :finish-on-release finish-on-release
                        :pointer pointer :multiple-window multiple-window))))

;; The CLIM 2.2 spec is slightly unclear about how the next two
;; functions are supposed to behave, especially wrt. the user
;; experience. I think these functions are supposed to present a
;; rectangle on screen and let the user drag around the edges - this
;; would make supporting both left/top and right/bottom make sense,
;; and provide a way for the :rectangle argument to
;; `pointer-input-rectangle' to make sense. However, this would be a
;; very weird user experience, so I (Troels) have instead chosen to
;; consider left/top and right/bottom to be the same thing, preferring
;; left/top if both are specified. The :rectangle argument to
;; `pointer-input-rectangle' is ignored. The user is meant to drag out
;; a rectangle with the mouse, possibly by first providing a starting
;; point. This is intuitive behavior and I see no point in supporting
;; something more complicated. These changes should be invisible to
;; the calling program.

(defun clime:pointer-input-rectangle* (&key (stream *standard-output*)
                                 (pointer (port-pointer (port stream)))
                                 multiple-window left top right bottom
                                 (finish-on-release t))
  "Let the user drag a rectangle on `stream' and return four
values, the coordinates of the rectangle. `Pointer' is the
pointer that will be tracked (the default should be used unless
the port has multiple pointing devices), `multiple-window' is
currently unimplemented and both `left'/`top' and
`right'/`bottom' specify an initial position for a rectangle
corner. You must provide either both parts of any of these two
coordinate pairs or none at all. If you provide both `left'/`top'
and `right'/`bottom', the `left'/`top' values will be used,
otherwise, the non-nil set will be used. If neither is specified,
the user will be able to specify the origin corner of the
rectangle by clicking the mouse. If `finish-on-release' is true,
the function will end when the user releases the mouse button. If
false, the user will have to click to finish inputting the
rectangle."
  (assert (not (eq (not (not top)) (not left))) nil
          "You must provide either none or both of `:top' and `:left'")
  (assert (not (eq (not (not right)) (not bottom))) nil
          "You must provide either none or both of `:right' and `:bottom'")
  (setf top (or top bottom)
        left (or left right))
  (unless top
    (block nil
      (tracking-pointer (stream :pointer pointer
                                :multiple-window multiple-window)
        (:pointer-button-press (event x y)
                               (declare (ignore event))
                               (setf left x)
                               (setf top y)
                               (return)))))
  (multiple-value-bind (x y)
      (labels ((draw (x y state)
                 (declare (ignore state))
                 (with-drawing-options (stream :ink +flipping-ink+)
                   (draw-rectangle* stream left top x y :filled nil))))
        (dragging-drawing stream #'draw :finish-on-release finish-on-release
                          :pointer pointer :multiple-window multiple-window))
    ;; Normalise so that x1 < x2 ^ y1 < y2.
    (values (min left x) (min top y)
            (max left x) (max top y))))

(defun clime:pointer-input-rectangle (&rest args &key (stream *standard-output*)
                                (pointer (port-pointer (port stream)))
                                multiple-window rectangle
                                (finish-on-release t))
  "Like `pointer-input-rectangle*', but returns a bounding
rectangle instead of coordinates."
  (declare (ignore pointer multiple-window rectangle finish-on-release))
  (climi::with-keywords-removed (args (:rectangle))
    (apply #'make-bounding-rectangle (multiple-value-list (apply #'pointer-input-rectangle* args)))))
