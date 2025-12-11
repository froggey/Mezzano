# Atomic extensions

## Atomic places

The following places support atomic operations:

* Special variables
* Lexical variables (TODO)
* Structure slots
* `car`
* `cdr`
* `svref`
* `aref`/`row-major-aref` (except on strings and arrays of complex numbers)
* `standard-instance-access`/`funcallable-standard-instance-access`
* `slot-value`
* `elt`
* `first`/`second`/.../`tenth`
* `rest`
* `nth`
* `gethash`

Further atomic places may be defined through cas functions.

## Functions & macros

### `cas`/`compare-and-swap`

`(compare-and-swap place old-value new-value)`

Atomically read the value of `place`, compare with `old-value`, if `eq` then
write `new-value` otherwise do nothing.

`place` must be an atomic place.

Returns the old value of the place.

### `double-compare-and-swap`

`(double-compare-and-swap place-1 place-2 old-1-value old-2-value new-2-value new-2-value)`

Atomically perform a compare-and-swap operation on two places. This is equivalent to
the x86-64 instruction `cmpxchg16b` or the arm64 instruction `casp`.

Due to machine limitations, the places must be physically contiguous in memory.

The only supported places are `car`/`cdr` (in either order) on the same cons, or
a `:dcas-sibling` pair within a struct (see [defstruct-extensions.md]).

Returns 3 values.
1. A generalized boolean indicating if the swap was successful.
2. The old value of `place-1`.
3. The old value of `place-2`.

### `get-cas-expansion`

`(get-cas-expansion place &optional environment)`

Returns a number of values describing how to perform cas operations on `place`,
similar to `get-setf-expansion`.

1. A list of temporary variables.
2. A list of value-forms whose results those variable must be bound.
3. Temporary variable for the old value of `place`.
4. Temporary variable for the new value of `place`.
5. Form using the aforementioned temporaries which performs the compare-and-swap operation on `place`.
6. Form using the aforementioned temporaries with which to perform an atomic read of `place`.
7. Form using the aforementioned temporaries with which to perform an atomic write of `place`.

### `atomic-incf`
### `atomic-decf`
### `atomic-logandf`
### `atomic-logiorf`
### `atomic-logxorf`
### `atomic-swapf`

## Orderings

FIXME: Atomic ordering is currently poorly defined. On arm64 all atomic ops are acqrel,
x86-64 the are seqcst.

Ideally we'd like to follow Rust's semantics and add success/failure orderings to cas
& the other atomic operations.

* `:seqcst` - Sequentially consistent
* `:acqrel` - Acquire/release
* `:acquire` - Acquire
* `:release` - Release
* `:relaxed` - Relaxed

## Custom cas functions

Further atomic places can be defined via cas functions, similar to setf functions.
The function must accept at least two values: the old value to compare against, and
the new value to write. As with setf functions, it may accept additional parameters.
It must return the existing value that was read from the place.

```lisp
;; Example implementation, that doesn't operate atomically.
(defun (cas my-cas-function) (old-value new-value place-parameters...)
  (let ((current-value (read place-parameters...)))
    (when (eql current-value old-value)
      (write new-value place-parameters...))
    current-value))
```
