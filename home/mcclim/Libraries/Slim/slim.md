# Simplified Lisp Interface Manager

This abstraction layer is provided to reduce cognitive and manual
effort of writing graphical user interface in CLIM at expense of using
abbreviations, depending on context and reducing level of control
accessible to the programmer.

## Usage

## API

### Setting context

`with-pane (pane) &body body`

To set context programmer has to point the stream pane which he
operates on. `with-pane` binds `slim:*pane*` and `*standard-output*`
to the supplied `pane` argument.

```common-lisp
(slim:with-pane (pane)
  (format slim:*pane* "hi1")
  (format t "hi2"))
```

### Defining application frames

`defapp name-and-opts args &body body`

Works like defun except that function starts the application frame and
executes the body as the frame display-function (when the frame is
redisplayed body will be run again). Binds `clim:*application-frame*`
and `*standard-output*`. All function arguments are available for the
display function. First argument may be a list, then first element is
a name and the rest are parameters used when creating the application
pane.

```common-lisp
(defapp (example :text-margins `(:left 20 :top 10 :right 20 :bottom 10))
    (x y z)
  (clim:draw-rectangle* *standard-output* x y (+ x 100) (+ y 200))
  (format t "~&Z is ~s" z))

(example 100 100 "jackdaniel")
```

### Tables

`with-table (&optional pane &rest options) &body body`

Creates context for printing output in a table. If `pane` is not
supplied, `slim:*pane*` has to be bound to the destination stream
pane. `options` are the same as for `clim:formatting-table`.

Output is organized by rows and columns with the following macros:

- `row &body body`
- `cell &body body`

Example:

```common-lisp
(defun display-pane (frame pane)
  (declare (ignore frame))
  (slim:with-pane (pane)
    (slim:with-table ()
      (slim:row
        (slim:cell (princ "Description:"))
        (slim:cell (princ "foo")))
      (slim:row
        (slim:cell (princ "Information:"))
        (slim:cell (slim:with-opts (clim:+red+ :bold :fixed)
                     (princ "bar"))))
      (when (= 3 3)
        (slim:row
          (slim:cell (princ "Extra:"))
          (slim:cell (format t "~A" 'quux)))))))
```

### Sheet and output-record protocols unification

Sheet and output record have separate protocols doing the same thing
for them. We provide a unified protocol which spans them with this
regard.

| unified function  | sheet function          | output-record function      |
|-------------------|-------------------------|-----------------------------|
| slim:parent       | clim:sheet-parent       | clim:output-record-parent   |
| slim:children     | clim:sheet-children     | clim:output-record-children |
| slim:add-child    | clim:sheet-adopt-child  | clim:add-output-record      |
| slim:delete-child | clim:sheet-disown-child | clim:delete-output-record   |
| slim:repaint      | clim:repaint-sheet      | clim:replay-output-record   |
