;;;; Copyright (c) 2018 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage :mezzano.cold-generator.x86-64
  (:use :cl)
  (:import-from #:mezzano.cold-generator
                #:configure-system-for-target)
  (:local-nicknames (#:env #:mezzano.cold-generator.environment)))

(in-package :mezzano.cold-generator.x86-64)

(defmethod configure-system-for-target (environment (target (eql :x86-64)))
  ;; TODO: Add the interrupt thunks.
  (env:add-special environment :undefined-function
                   (env:compile-lap environment
                                    cold-generator.x86-64:*undefined-function-thunk*
                                    :area :wired
                                    :name (env:translate-symbol environment 'sys.int::%%undefined-function-trampoline)))
  (env:add-special environment :closure-trampoline
                   (env:compile-lap environment
                                    cold-generator.x86-64:*closure-trampoline*
                                    :area :wired
                                    :name (env:translate-symbol environment 'sys.int::%%closure-trampoline)))
  (env:add-special environment :funcallable-instance-trampoline
                   (env:compile-lap environment
                                    cold-generator.x86-64:*funcallable-instance-trampoline*
                                    :area :wired
                                    :name (env:translate-symbol environment 'sys.int::%%funcallable-instance-trampoline))))
