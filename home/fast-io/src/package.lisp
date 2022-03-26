(defpackage :fast-io
  (:use #:cl #:alexandria #:trivial-gray-streams)
  (:export #:*default-output-buffer-size*

           #:octet #:octet-vector #:index
           #:input-buffer #:output-buffer

           #:make-octet-vector #:octets-from

           #:make-output-buffer #:finish-output-buffer
           #:buffer-position

           #:make-input-buffer #:input-buffer-vector #:input-buffer-stream

           #:fast-read-byte #:fast-write-byte
           #:fast-read-sequence #:fast-write-sequence
           #:with-fast-input #:with-fast-output

           ;#:fast-seek

           #:write8 #:writeu8
           #:write8-le #:writeu8-le #:write8-be #:writeu8-be
           #:write16-le #:writeu16-le #:write16-be #:writeu16-be
           #:write24-le #:writeu24-le #:write24-be #:writeu24-be
           #:write32-le #:writeu32-le #:write32-be #:writeu32-be
           #:write64-le #:writeu64-le #:write64-be #:writeu64-be
           #:write128-le #:writeu128-le #:write128-be #:writeu128-be

           #:read8 #:readu8
           #:read8-le #:readu8-le #:read8-be #:readu8-be
           #:read16-le #:readu16-le #:read16-be #:readu16-be
           #:read32-le #:readu32-le #:read32-be #:readu32-be
           #:read64-le #:readu64-le #:read64-be #:readu64-be
           #:read128-le #:readu128-le #:read128-be #:readu128-be

           #:fast-output-stream #:fast-input-stream
           #:finish-output-stream))
