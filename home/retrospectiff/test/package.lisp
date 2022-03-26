;;; Copyright (c) 2011 Cyrus Harmon, All rights reserved.
;;; See COPYRIGHT file for details.

(cl:defpackage :retrospectiff-test
  (:use #:cl #:retrospectiff #:fiveam))

(cl:in-package #:retrospectiff-test)

(def-suite :retrospectiff)


(cl:defpackage :retrospectiff2-test
  (:use #:cl #:retrospectiff2 #:fiveam))

(cl:in-package #:retrospectiff2-test)

(def-suite :retrospectiff2)

