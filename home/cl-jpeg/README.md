# CL-JPEG
Baseline JPEG codec written in Common Lisp. Up-to-date source is hosted on [GitHub](https://github.com/sharplispers/cl-jpeg).

Written by Eugene Zaikonnikov, 1999-2017. Valuable contributors, in no particular order: Henry Harrington, Kenan Bölükbaşı, Manuel Giraud, Cyrus Harmon and William Halliburton.

## Image Format

* B, G, R pixels in case of colorspace-converted three component image
* Y, Cb, Cr pixels in case where colorspace conversion of three component image was disabled
* array of grayscale pixels in case of single component
* array of 2 or 4 pixels in the case of two or four component image respectively


## Decoding

### `(DECODE-IMAGE FILENAME &KEY BUFFER (COLORSPACE-CONVERSION T) CACHED-SOURCE-P)`
* FILENAME - jpeg file name
* BUFFER - optional reusable storage for output image
* COLORSPACE-CONVERSION controls if YUV to RGB should be performed
* CACHED-SOURCE-P - the file input layer behaviour

Returns (multiple-valued) decoded IMAGE array in the same format as encoder source image, image HEIGHT and image WIDTH.

### `(DECODE-STREAM STREAM &KEY BUFFER (COLORSPACE-CONVERSION T) DESCRIPTOR CACHED-SOURCE-P)`
* STREAM - jpeg image stream or NIL in case where the buffer is pre-allocated in DESCRIPTOR
* BUFFER - optional reusable storage for output image
* COLORSPACE-CONVERSION controls if YUV to RGB should be performed
* DESCRIPTOR is a JPEG:DESCRIPTOR structure. Can be reused for performance reasons bulk processing applications
* CACHED-SOURCE-P - the file input layer behaviour

Returns (multiple-valued) decoded IMAGE array in the same format as encoder source image, image HEIGHT and image WIDTH.

### `(ALLOCATE-BUFFER HEIGHT WIDTH NCOMP)`
* HEIGHT - image height
* WIDTH - image width
* NCOMP - number of image components

Allocates and retruns a buffer suitable for storage of the output image in the specified geometry.

### `(JPEG-FILE-DIMENSIONS FILENAME)`
* FILENAME - the file to be probed

Returns image height, width, number of components, and the type of Adobe colorpsace transform encoded in the image (:YCBCR-RGB, :YCCK-CMYK).

### `(CONVERT-CMYK-TO-RGB BUFFER H W &key RGB-BUFFER)`
* BUFFER - the input CMYK pixel buffer
* H,W - image dimensions
* RGB-BUFFER - optional reusable storage for output RGB image

Returns an RGB conversion of the supplied CMYK image.

###`(JPEG-TO-BMP &key INFILE OUTFILE)`

Converts JPEG file to BMP file.

## Encoding

### `(ENCODE-IMAGE FILENAME IMAGE NCOMP H W &key SAMPLING Q-TABS Q-FACTOR)`
* FILENAME - output file name
* IMAGE - input array of pixels
* NCOMP - number of components in the image
* H, W - image dimensions
* SAMPLING - sampling frequency for ncomp components by X and Y axis, e.g. '((2 2) (1 1) (1 1)) for three components, can be omitted for grayscale and RGB images
* Q-TABS - specifies quantization tables vector, should be 1 for 1, 2 for 2, 2 for 3 and 4 entries for 4 components
* Q-FACTOR - quality specifier (1-64), default is 64

Encodes the supplied image using the sampling specifier, quantization tables and quantization factor.

### `(ENCODING-WRAPPER FILENAME IMAGE NCOMP H W &key (QUALITY 4))`
* FILENAME - output file name
* IMAGE - input array of pixels
* NCOMP - number of components in the image
* H, W - image dimensions
* QUALITY - quality factor on scale from 1 to 5

## TODO

* Add progressive JPEG support in decoder
