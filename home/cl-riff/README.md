cl-riff
=======

The Resource Interchange File Format (RIFF) is a generic file
container format for storing data in tagged chunks. It is primarily
used to store multimedia such as sound and video, though it may also
be used to store any arbitrary data.

I wrote this code as a cross-platform way of processing WAV audio
files. (See cl-wav).

Use read-riff-file to load a whole file, returning it's content as a
list of chunks, where each chunk is a plist.

Alternatively, open the stream yourself and call read-chunk
successively until NIL.

Chunks are represented as plists, with accessor methods defined for
chunk-id, chunk-data-size, chunk-data and file-type.

Example
-------

	> (ql:quickload :cl-riff)
	> (riff:read-riff-file "c:/windows/media/ding.wav")
	...
	> (riff:riff-file-type (first *))
	"WAVE"

    > (riff:write-riff-file (riff:read-riff-file "c:/windows/media/ding.wav")
                            "c:/windows/media/ding-copy.wav")

References
----------

[Resource Interchange File Format, Wikipedia](http://en.wikipedia.org/wiki/Resource_Interchange_File_Format)

[Resource Interchange File Format Services, MSDN](http://msdn.microsoft.com/en-us/library/windows/desktop/dd798636(v=vs.85).aspx)

[Resource Interchange File Format (RIFF), MSDN](http://msdn.microsoft.com/en-us/library/windows/desktop/ee415713(v=vs.85).aspx)


Contributors
------------

Write functionality by Patrick Stein.
