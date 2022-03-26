# fast-io

**Now with
[static-vectors](https://github.com/sionescu/static-vectors)
support!**

```lisp
(deftype octet '(unsigned-byte 8))
(deftype octet-vector '(simple-array octet (*)))
```

Fast-io is about improving performance to octet-vectors and octet
streams (though primarily the former, while wrapping the latter).
Imagine we're creating messages for the network. If we try and fill an
octet-vector with 50 bytes, 50000 times, here are the results (SBCL
1.0.57):

<table>
<tr>
  <th></th>
  <th align=right><tt>vector-push-extend</tt>:</th>
  <th align=right><tt>flexi-streams</tt>:</th>
  <th align=right><tt>fast-io</tt>:</th>
</tr>
<tr>
  <td align=right>Time:</td>
  <td align=right>0.767s</td>
  <td align=right>2.545s</td>
  <td align=right>0.090s</td>
</tr>
<tr>
  <td align=right>Bytes consed:</td>
  <td align=right>104,778,352</td>
  <td align=right>274,452,768</td>  
  <td align=right>18,373,904</td>
</tr>
</table>

(See `t/benchmarks.lisp` for the exact code used.)

It *should* be surprising that it takes a nontrivial effort to achieve
relatively decent performance to octet-vectors, but probably isn't.
However, fast-io provides a relatively straightforward interface for
reading and writing either a stream or a vector:

```lisp
;;; Write a byte or sequence, optionally to a stream:

(with-fast-output (buffer [STREAM | :vector | :static])
  (fast-write-byte BYTE buffer))

(with-fast-output (buffer [STREAM | :vector | :static])
  (fast-write-sequence OCTET-VECTOR buffer [START [END]]))

;;; Read from a vector or stream:

(with-fast-input (buffer VECTOR [STREAM])
  (fast-read-byte buffer))

(with-fast-input (buffer VECTOR [STREAM])
  (let ((vec (make-octet-vector N)))
    (fast-read-sequence vec buffer [START [END]])))
```

## Multi-byte and Endianness

Fast-io provides a host of read and write functions for big- and little-endian reads.  See the [Dictionary](#reading-and-writing) below.

## Static Vectors

You may now specify `:static` instead of a stream to
`WITH-OUTPUT-BUFFER`.  This returns an octet-vector created with
[static-vectors](https://github.com/sionescu/static-vectors),
which means that passing the buffered data directly to a foreign
function is now that much more efficient:

```lisp
(let ((data (with-fast-output (buffer :static)
              (buffer-some-data buffer))))
  (foreign-send (static-vectors:static-vector-pointer data))
  (static-vectors:free-static-vector data))
```

Note that the restriction for manually freeing the result remains.
This avoids multiple inefficient (i.e., byte-by-byte) copies to
foreign memory.

## Streams

Obviously, the above API isn't built around Lisp streams, or even
gray-streams.  However, fast-io provides a small wrapper using
`trivial-gray-streams`, and supports `{WRITE,READ}-SEQUENCE`:

```lisp
(let ((stream (make-instance 'fast-io:fast-output-stream)))
  (write-sequence (fast-io:octets-from '(1 2 3 4)) stream))
```

Both `fast-input-stream` and `fast-output-stream` support backing a
stream, much like using the plain fast-io buffers.  However, using the
gray-streams interface is a 3-4x as slow as using the buffers alone.
Simple benchmarks show the gray-streams interface writing 1M 50-byte
vectors in about 1.7s, whereas simply using buffers is about 0.8s.
Consing remains similar between the two.

## Dictionary

### Octets

Most functions operate on or require octet-vectors, i.e.,

```lisp
(deftype octet () '(unsigned-byte 8))
(deftype octet-vector '(simple-array octet (*)))
```

Which is exactly what is defined and exported from `fast-io`.  Also:

* `make-octet-vector LEN`<br> Make an octet-vector of length `LEN`.
* `octets-from SEQUENCE`<br> Make an octet-vector from the contents of `SEQUENCE`.

### Buffers

* `make-input-buffer &key VECTOR STREAM POS`<br> Create an input buffer for use with input functions.  `:vector` specifies the vector to be read from.  `:stream` specifies the stream to read from.  `:pos` specifies the offset to start reading into `VECTOR`.
* `make-output-buffer &key OUTPUT`<br> Create an output buffer for use with output functions. `:output` specifies an output stream.  If `:output :static` is specified, and static-vectors is supported, output will be to a static-vector.
* `finish-output-buffer BUFFER`<br> Finish the output and return the complete octet-vector.
* `buffer-position BUFFER`<br> Return the current read/write position for `BUFFER`.

* `with-fast-input (BUFFER VECTOR &optional STREAM (OFFSET 0)) &body body`<br> Create an input buffer called `BUFFER`, optionally reading from `VECTOR`, followed by reading from `STREAM`.  If `OFFSET` is specified, start reading from this position in `VECTOR`.
* `with-fast-output (BUFFER &optional OUTPUT) &body BODY`<br> Create an output buffer named `BUFFER`, optionally writing to the stream `OUTPUT`.  This will automatically `FINISH-OUTPUT-BUFFER` on `BUFFER`.  Thus the `with-fast-output` form evaluates to the completed octet-vector.

### Reading and Writing

* `fast-read-byte INPUT-BUFFER &optional (EOF-ERROR-P t) EOF-VALUE`<br> Read a byte from `INPUT-BUFFER`.  If `EOF-ERROR-P` is `t`, reading past the end-of-file will signal `CL:END-OF-FILE`.  Otherwise, it will return `EOF-VALUE` instead.
* `fast-write-byte BYTE OUTPUT-BUFFER`<br> Write a byte to `OUTPUT-BUFFER`.
* `fast-read-sequence SEQUENCE INPUT-BUFFER &optional (START 0) END`<br> Read from `INPUT-BUFFER` into `SEQUENCE`.  Values will be written starting at position `START` and, if `END` is specified, ending at `END`.  Otherwise values will be written until the length of the sequence, or until the input is exhausted.
* `fast-write-sequence SEQUENCE OUTPUT-BUFFER &optional (START 0) END`<br> Write `SEQUENCE` to `OUTPUT-BUFFER`, starting at position `START` in `SEQUENCE`.  If `END` is specified, values will be written until `END`; otherwise, values will be written for the length of the sequence.

For multi-byte reads and writes requiring endianness, fast-io provides functions in the following forms:

* `write[u]{8,16,32,64,128}{-be,-le}`: E.g., `(write32-be VALUE BUFFER)` will write the specified 32-bit value to the specified buffer with a *big-endian* layout.  Likewise, `(writeu16-le VALUE BUFFER)` will write an *unsigned* 16-bit value in *little-endian* layout.
* `read[u]{8,16,32,64,128}{-be,-le}`: Similarly, `(read64-le BUFFER)` will read a 64-bit value from the buffer with little-endian layout.
