# CL-TGA

[![Quicklisp](http://quickdocs.org/badge/cl-tga.svg)](http://quickdocs.org/cl-tga/)

Cl-tga was written to facilitate loading .tga files into OpenGL programs.  It's a very simple library, and, at the moment, only supports non-RLE encoded forms of the files, though it shouldn't be hard to fix that.

Cl-tga does *not* re-order the color bytes from the file body.  tga files are econded in bgr byte-order, and so loading code using cl-opengl should specify the :bgr or :bgra data format in order to not have wonky looking textures, eg.:

```
(gl:tex-image-2d :texture-2d 0 :rgba
		 (tga:image-width image) (tga:image-height image)
		 0
		 (ecase (tga:image-channels image)
		   (3 :bgr)
	  	   (4 :bgra))
		 :unsigned-byte (tga:image-data image))
```
## Dictionary

### read-tga *filespec* => tga
Takes a filespec and returns a cl-tga::tga instance that contains the file data

### image-height, image-width, image-channels, image-bpp, image-data
All of these functions take only a cl-tga::tga instance and return properties of it:

height: height of the image

width: width of the image

channels: 3 for rgb, 4 for rgba

bpp: bytes per pixel, will be (member 16 24 32)

data: The raw image data encoded as a (vector (unsigned-byte 8)) in bgr(a) color order
