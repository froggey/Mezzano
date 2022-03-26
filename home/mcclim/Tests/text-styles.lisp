;;;;  (c) copyright 2020 Jan Moringen
;;;;  (c) copyright 2020 Daniel Kochmański
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Library General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Library General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Library General Public
;;;; License along with this library; if not, write to the
;;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;;; Boston, MA  02111-1307  USA.

(in-package #:clim-tests)

(def-suite* :mcclim.text-styles
  :in :mcclim)

(test parse-text-style.smoke
  "Smoke test for the `parse-text-style' function."

  (mapc (lambda (input-and-expected)
          (destructuring-bind (input &optional (expected t))
              input-and-expected
            (case expected
              ((t :no-op)
               (let ((result (climi::parse-text-style input)))
                 (is-true (text-style-p result))
                 (when (eq expected :no-op)
                   (is (eq input result)))))
              (error
               (signals error (climi::parse-text-style input))))))
        `(;; Invalid text style specifications
          (1                 error)
          ((nil nil)         error)
          ((nil nil nil nil) error)
          ;; Valid ones
          (nil)
          ((nil nil nil))
          ((:fix nil 22))
          ;; Must be returned as-is
          (,(make-text-style nil nil nil) :no-op)
          (,(make-text-style :fix nil nil) :no-op)
          (,(make-text-style :fix :roman :small) :no-op)
          (,(make-text-style nil nil :smaller) :no-op)
          (,(make-text-style nil nil :larger) :no-op)
          (,(make-text-style :fix :roman 10) :no-op))))
