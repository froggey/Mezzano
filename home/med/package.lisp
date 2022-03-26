(defpackage :med
  (:use :cl)
  (:export #:spawn #:open-file-request
           #:find-file
           #:file-completer
           #:read-from-minibuffer
           #:*editor*
           #:buffer
           #:current-buffer
           #:last-buffer
           #:switch-to-buffer
           #:buffer-list
           #:buffer-property
           #:buffer-modified
           #:buffer-point
           #:move-beginning-of-buffer
           #:move-end-of-buffer
           #:character-right-of
           #:character-left-of
           #:insert))
