# Video file decoding in Common Lisp

Simple video decoder written in Common Lisp. AVI/MJPEG playback leverages [CL-JPEG](https://github.com/sharplispers/cl-jpeg) for frame processing and [CL-RIFF](https://github.com/RobBlackwell/cl-riff) for container format handling. It also supports GIF playback via [Skippy](https://github.com/xach/skippy).

A primitive CLX media player is included. Playback of PCM encoded audio streams is supported.

Has only lightly tested on SBCL 13.x/Linux x86-64, CCL 1.11 and Mezzano. CL-JPEG version 2.8 or higher is required.

Some sample files can be found [here](https://cinelerra-cv.org/footage.php) (the toy plane AVI) and [here](http://jjc.freeshell.org/turning_pages.html).


## Known Limitations

* No indexing support

## TODO:

* AVI MJPEG chunk decoding [done]
* Rudimentary video stream player [done]
* Indexing support
* Multicore frame decoding
