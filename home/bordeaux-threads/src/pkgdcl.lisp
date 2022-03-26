;;;; -*- indent-tabs-mode: nil -*-

(cl:defpackage :bordeaux-threads
  (:nicknames #:bt)
  (:use #:cl #:alexandria)
  #+abcl
  (:import-from :java #:jnew #:jcall #:jmethod)
  (:export #:thread #:make-thread #:current-thread #:threadp #:thread-name
           #:start-multiprocessing
           #:*default-special-bindings* #:*standard-io-bindings*
           #:*supports-threads-p*

           #:lock #:make-lock #:lock-p
           #:acquire-lock #:release-lock #:with-lock-held

           #:recursive-lock #:make-recursive-lock #:recursive-lock-p
           #:acquire-recursive-lock #:release-recursive-lock #:with-recursive-lock-held

           #:make-condition-variable #:condition-wait #:condition-notify

           #:with-timeout #:timeout

           #:all-threads #:interrupt-thread #:destroy-thread #:thread-alive-p
           #:join-thread #:thread-yield)
  (:documentation "BORDEAUX-THREADS is a proposed standard for a minimal
  MP/threading interface. It is similar to the CLIM-SYS threading and
  lock support, but for the following broad differences:

  1) Some behaviours are defined in additional detail: attention has
     been given to special variable interaction, whether and when
     cleanup forms are run. Some behaviours are defined in less
     detail: an implementation that does not support multiple
     threads is not required to use a new list (nil) for a lock, for
     example.

  2) Many functions which would be difficult, dangerous or inefficient
     to provide on some implementations have been removed. Chiefly
     these are functions such as thread-wait which expect for
     efficiency that the thread scheduler is written in Lisp and
     'hookable', which can't sensibly be done if the scheduler is
     external to the Lisp image, or the system has more than one CPU.

  3) Unbalanced ACQUIRE-LOCK and RELEASE-LOCK functions have been
     added.

  4) Posix-style condition variables have been added, as it's not
     otherwise possible to implement them correctly using the other
     operations that are specified.

  Threads may be implemented using whatever applicable techniques are
  provided by the operating system: user-space scheduling,
  kernel-based LWPs or anything else that does the job.

  Some parts of this specification can also be implemented in a Lisp
  that does not support multiple threads. Thread creation and some
  thread inspection operations will not work, but the locking
  functions are still present (though they may do nothing) so that
  thread-safe code can be compiled on both multithread and
  single-thread implementations without need of conditionals.

  To avoid conflict with existing MP/threading interfaces in
  implementations, these symbols live in the BORDEAUX-THREADS package.
  Implementations and/or users may also make them visible or exported
  in other more traditionally named packages."))
