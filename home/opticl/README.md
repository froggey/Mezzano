# opticl: A library for representing and processing images in Common Lisp (CL)

By Cyrus Harmon <ch-lisp@bobobeach.com>, February 2011. See COPYRIGHT
file for license details.

# opticl-core

NOTE: If you are using this version (or later) of opticl, you now
also need [opticl-core](https://github.com/slyrus/opticl-core).

# Overview

opticl is designed to be a high-performance, but relatively
lightweight, library for representing, processing, loading, and saving
2-dimensional pixel-based images. opticl aims to improve upon my first
attempt at an image processing library -- ch-image, and also borrows
some ideas from Matthieu Villeneuve's excellent imago image processing
library. Representing and processing images provides an excellent
illustration of the trade-offs between generality, and complexity, on
the one hand, and simplicity and efficiency, on the other hand. All
other things being equal, one generally wants a simple system that is
both efficient and general enough to be suitable for use in a variety
of different contexts -- opticl aims to strike this balance and to be
both broadly applicable in different contexts and to provide a core
set of functionality that is of high-enough performance to be useful
in time-(and resource-)sensitive operations.

# Installation

The easiest way to install opticl is to use Zachary Beane's fabulous
quicklisp library:

    (ql:quickload 'opticl)

# For the Impatient

For a quick example, let's load an image (of a truck) from a JPEG
file, invert the red channel and save the image back out to another
jpeg file:


    (defpackage #:impatient (:use #:cl #:opticl))
    (in-package #:impatient)

    (let ((img (read-jpeg-file "test/images/truck.jpeg")))
      (typecase img
        (8-bit-rgb-image
         (locally
             (declare (type 8-bit-rgb-image img))
           (with-image-bounds (height width)
               img
             (time
              (loop for i below height
                 do (loop for j below width 
                       do 
                       (multiple-value-bind (r g b)
                           (pixel img i j)
                         (declare (type (unsigned-byte 8) r g b))
                         (setf (pixel img i j)
                               (values (- 255 r) g b))))))))))
      (write-jpeg-file "test/output/inv-r-truck.jpeg" img))


If we time the `(loop for i below...)` using the time macro, with SBCL we see
the following:

    Evaluation took:
      0.006 seconds of real time
      0.005708 seconds of total run time (0.005538 user, 0.000170 system)
      100.00% CPU
      11,694,688 processor cycles
      0 bytes consed

Which shows that we're able to perform simple arithmetic operations on
each pixel of the image in 6 milliseconds, and that we don't need to
cons to do so.

# Image Representation

In ch-image, images were represented by a set of CLOS classes which,
in turn, either extended or wrapped classes from the CLEM
matrix-processing library. The idea was that CLEM could do the heavy
lifting and ch-image could take advantage of CLEM's relatively
efficient routines for storing arrayed sets of 2-dimensional
numbers. This worked reasonably well, and allowed for ch-image to have
a great variety of, at least conceptual, image types, such as various
sizes of RGB and grayscale images, multichannel images, floating point
images, binary images, etc..., but this approach had to fundamental
costs. First, it required that client programs wishing to use ch-image
use CLEM as well -- and CLEM brings along a host of other things that
may not be desired by the image-library-using programmer. Second, and
more problematic, it relied on CLEM's facilities for accessing image
data, or digging deeply into CLEM data structures to get access to the
underlying data, which seems to be missing the point.

So... I've taken a different approach with opticl, which is to largely
eschew CLOS classes and to provide the image data directly as native
CL arrays. Clearly, some measure of abstraction can be useful to
insulate programmers from subsequent changes in the implementation,
but this abstraction should be kept to a minimum and should not get in
the way of programmers seeking to use the data. Therefore, the
fundamental data structure of opticl is the CL array, but the API to
create and access the data in these arrays is a set of functions that
are used to make images and to get and set the data in the
images. These functions are implemented as non-generic functions,
which can be inlined (with a sufficiently smart compiler) for
efficient access to image data. To date, opticl has only been tested
on SBCL, and, conversely, has been designed to exploit the
performance-enhancing characteristics of the SBCL compiler, such as
efficient access to specialized arrays (given proper type
declarations). opticl contains CL types (not classes) and the core
functions for creating and accessing and setting pixel values use
these type declarations to enable SBCL to generate relatively
efficient code for accessing image data.

## Multi-dimensional Arrays

Common Lisp's multidimensional arrays provide some attractive
qualities for representing images. At the core, it is desirable to
have a representation that lends itself to efficient operations --
many languages offer high performance one-dimensional array access,
and some offer efficient access to multidimensional arrays. However,
merely the bytes that comprise the underlying array may not be
sufficient for one to intelligently use the array. But the bytes that
make up the image are only part of the story, the other critical
pieces are the data that describes the bytes in those arrays, the
dimensions of the image, the number of image channels, etc... In
ch-image I used CLOS classes for this data and for holding a reference
to the underlying pixels themselves. Fortunately, CL's array type
itself enables us to store this metadata directly in a
multidimensional array. We define a mapping between various image
types and various specialized CL array types, such that, for instance,
an 8-bit RGB array is represented by the type `(SIMPLE-ARRAY
(UNSIGNED-BYTE 8) (* * 3))`. Any 3-dimensional simple-array with a
third dimension of size 3 and an element-type of `(unsigned-byte 8)`
will satisfy the conditions of being an `8-bit-rgb-image`.

This enables both opticl code and user code to infer the dimensions
and the kind of pixels represented in a(n appropriate) CL array that
happens to be on opticl `image`. This, in turn, allows for both opticl
code and user code to use type declarations to enable the compiler to
generate high-performance code for processing images. It is chiefly
this facility that distinguishes opticl from other CL image processing
libraries such as ch-image and imago.

## Multiple Values

Another facility afforded by CL, is the notion of multiple values. If
one wants to represent a pixel of an 8-bit RGB image, and to perform
an operation on the individual color values of this pixel, one is
presented with a number of alternatives. Without using
multiple-values, one can treat the pixel as a single 24-bit unsigned
integer, knowing which bits correspond to the red, green and blue
channels; one can get the values as a list of three 8-bit integers; or
one can rely on reader/writer functions. Each of these alternatives
has some drawbacks.

The 24-bit unsigned integer approach is relatively clean, but requires
that user code unpack the image into it's respective components. Easy
enough to do, but we just lost two things. First, the image would now
be represented as an array of unsigned 24-bit integers -- or in the
case of an RGBA image, unsigned 32-bit integers. How would one
distinguish this from a 32-bit grayscale image? One would need
additional information. Second, one would be relying on either user
code or library-provided facilities for unpacking the color
information. It is my belief that the compiler is going to do at least
as good of a job as user code in pulling those values out of an
additional array dimension than user or library code would. On the
other hand, using a list or reader/writer functions would likely
involve heap-allocation of data structures to store this information.

CL offers a facility that has the potential to alleviate these issues,
which is `multiple-values`. This allows us to return multiple (perhaps
stack-allocated) values from a function and for us to to efficiently
update the values in multiple places using `setf`. Furthermore, it
allows for a unified treatment of grayscale and RGB pixels as a
grayscale pixel is just a single value, while an RGB pixel is
represented by multiple values, as opposed to treating grayscale
values as an integer and RGB values as a list of integers. All of this
would just be theoretical navel-gazing if the implementations didn't
take advantage of the features of multiple values to provide efficient
compiled implementations of code that uses these
features. Fortunately, SBCL's implementation of multiple-values allows
us to define (possibly inline) reader and writer functions that can
access the pixel and color-value data efficiently and without
allocating additional memory on the heap (consing).

The trade-off in this approach is that doing so requires that we know
the kind of image with which are dealing, at least if we want to do so
efficiently. Fortunately, CL's type system gets us most of the way
there. I say most of the way there, as there is one limitation in
standard, which we will see in a moment. In the example above you'll
notice a line which reads:

    (declare (type 8-bit-rgb-image img))

This declaration tells the compiler that the variable image is of the
type 8-bit-rgb-image and the compiler is able to optimize the code
effectively. The problem is that this is great for things inside the
compiler, the compiler sees the declaration and can act accordingly,
but only the compiler can do so. In CL, these declarations are opaque
to the user/library programmer. This limitation wasn't lost on the
early CL implementors, but facilities for inspecting declarations
didn't make it into the CL spec, but rather, eventually, found there
way into the less-widely implemented Common Lisp the Lanuage, 2nd
Edition (CLtL2) book by Guy Steele. SBCL has a contrib library called
sb-cltl2 that provides the key facility we need,
`cltl2:variable-information`. We can use that function in code called
from our define-setf-expander, as shown below in get-image-dimensions,
to see if there is a declaration in effect:

    (defun get-image-dimensions (image-var env)
      (multiple-value-bind (binding-type localp declarations)
          (cltl2:variable-information image-var env)
        (declare (ignore binding-type localp))
        (let ((type-decl (find 'type declarations :key #'car)))
          (and type-decl
               (listp type-decl)
               (= (length type-decl) 4)
               (fourth type-decl)))))

This allows us to glean information from the information provided to
the compiler that enables opticl to efficiently operate on its images,
when given appropriate declarations, and still work, albeit less
efficiently, in the absence of the appropriate type declarations.

(Note: I still need to look into the availability of the CLtL2
functionality on other CL implementations.)

It is the representation of image data as native CL arrays and the
efficient performance of these reader and writer functions that offer
the hope that opticl can serve as a general purpose image processing
library suitable for use by a wide variety of CL programs.

# Supported File Formats

* PNG
* JPEG
* TIFF
* PBM
* PNM
* GIF

# Dependencies

While opticl is designed to have minimal dependencies, I have decided
that it is better to use existing libraries, where possible,
especially for file I/O of various formats. In ch-image, I tried to
make the file I/O sections optional dependencies, but this proved
merely to sow confusion into the minds of the user. With the advent of
quicklisp, dependencies on libraries that are in quicklisp are much
less painful (for the quicklisp user anyway) than they used to be.

* alexandria
* retrospectiff (new version -- as of??)
* com.gigamonkeys.binary-data (also known as monkeylib-binary-data)
* ieee-floats
* zpng
* salza2
* png-read
* iterate
* chipz
* babel
* cl-jpeg
* skippy

opticl and all of its dependencies should be automatically installed
by:

    (ql:quickload 'opticl)

# opticl-core

The new [opticl-core](https://github.com/slyrus/opticl-core) package
now contains the core machinery for representing images and accessing
and setting pixel data. It is now required for opticl.

# opticl-more-test and opticl-examples

In the interest of keeping the core opticl library small, I've split
off some test code in into
[opticl-more-test](https://github.com/slyrus/opticl-more-test) and
more expository example code into
[opticl-examples](https://github.com/slyrus/opticl-examples).

# Examples

Some examples of using opticl code can be found here:

[https://github.com/slyrus/opticl-examples](https://github.com/slyrus/opticl-examples)

# Contributors

Thanks to Ivan Chernetsky for contributing code thresholding grayscale images
