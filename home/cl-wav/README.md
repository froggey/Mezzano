cl-wav
======

Waveform audio file format (WAV files) uses Resource Interchange File
Format (RIFF) to store audio.

This project provides a cross platform reader for the WAV file format,
implemented in Common Lisp.

Optionally, you can customise the chunk data reader, for example see
wrap-data-chunk-data-samples-reader to read the data as an array of
float samples.

N.B. The data from WAV files can be quite long and can be *very* slow
to print out in a REPL. You probably don't want to do that.

For large files or streamed files, you can use cl-riff read-riff-chunk
to read chunks on demand, whilst still using the chunk-data-readers
provided here.

N.B. cl-wav has only been tested with a limited range of WAV files.

Example
-------

	> (ql:quickload :cl-wav)
	> (wav:read-wav-file "c:/windows/media/ding.wav")
	...
	> (second *)
	(:CHUNK-ID "fmt " :CHUNK-DATA-SIZE 16 :CHUNK-DATA
	(:COMPRESSION-CODE 1 :NUMBER-OF-CHANNELS 2 :SAMPLE-RATE 44100
	:AVERAGE-BYTES-PER-SECOND 176400 :BLOCK-ALIGN 4 :SIGNIFICANT-BITS-PER-SAMPLE
	16))
	> (wav:read-wav-file "c:/windows/media/ding.wav" :chunk-data-reader (wav:wrap-data-chunk-data-samples-reader))
	...
	
Limitations
-----------

Currently only supports reading files, not writing.

References
----------

[WAV, Wikipedia](http://en.wikipedia.org/wiki/WAV)

[Wave File Format, The Sonic Spot](http://www.sonicspot.com/guide/wavefiles.html)

Rob Blackwell    
February 2014
