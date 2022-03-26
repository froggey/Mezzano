(in-package :parsley)

(defun uncompress-bzip2 (octet-vector)
  (%uncompress-octets octet-vector 'chipz:bzip2))

(defun uncompress-gzip (octet-vector)
  (%uncompress-octets octet-vector 'chipz:gzip))

(defun uncompress-zlib (octet-vector)
  (%uncompress-octets octet-vector 'chipz:zlib))

(defun uncompress-deflate (octet-vector)
  (%uncompress-octets octet-vector 'chipz:deflate))
