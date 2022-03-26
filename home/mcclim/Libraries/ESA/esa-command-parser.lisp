;;; -*- Mode: Lisp; Package: ESA -*-

;;;  (c) copyright 2006 by
;;;           Christophe Rhodes (c.rhodes@gold.ac.uk)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

(in-package :esa)

;;; There is an ambiguity over what to do for parsing partial commands
;;; with certain values filled in, as might occur for keyboard
;;; shortcuts.  Either the supplied arguments should be treated as
;;; gospel and not even mentioned to the user, as we do now; or they
;;; should be treated as the default, but the user should be prompted
;;; to confirm, as we used to do.
(defun esa-parse-one-arg (stream name ptype accept-args)
  (declare (ignore name))
  ;; this conditional doesn't feel entirely happy.  The issue is that
  ;; we could be called either recursively from an outer call to
  ;; (accept 'command), in which case we want our inner accept to
  ;; occur on the minibuffer stream not the input-editing-stream, or
  ;; from the toplevel when handed a partial command.  Maybe the
  ;; toplevel should establish an input editing context for partial
  ;; commands anyway?  Then ESA-PARSE-ONE-ARG would always be called
  ;; with an input-editing-stream.
  (let ((stream (if (encapsulating-stream-p stream)
                    (encapsulating-stream-stream stream)
                    stream)))
    (apply #'accept (eval ptype)
           :stream stream
           ;; This is fucking nuts.  FIXME: the clim spec says
           ;; ":GESTURE is not evaluated at all".  Um, but how are you
           ;; meant to tell if a keyword argument is :GESTURE, then?
           ;; The following does not actually allow variable keys:
           ;; anyone who writes (DEFINE-COMMAND FOO ((BAR 'PATHNAME
           ;; *RANDOM-ARG* ""))) and expects it to work deserves to
           ;; lose.
           ;;
           ;; FIXME: this will do the wrong thing on malformed accept
           ;; arguments, such improper lists or those with an odd
           ;; number of keyword arguments.  I doubt that
           ;; DEFINE-COMMAND is checking the syntax, so we probably
           ;; should.
           (loop for (key val) on accept-args by #'cddr
                 unless (eq key :gesture)
                 collect key and collect (eval val)))))

(defun esa-command-parser (command-table stream)
  (let ((command-name nil))
    (flet ((maybe-clear-input ()
             (let ((gesture (read-gesture :stream stream 
                                          :peek-p t :timeout 0)))
               (when (and gesture (or (delimiter-gesture-p gesture)
                                      (activation-gesture-p gesture)))
                 (read-gesture :stream stream)))))
      (with-delimiter-gestures (*command-name-delimiters* :override t)
        ;; While reading the command name we want use the history of
        ;; the (accept 'command ...) that's calling this function.
        ;;
        ;; FIXME: does this :history nil actually achieve the above?
        (setq command-name (accept `(command-name :command-table ,command-table)
                                   :stream (encapsulating-stream-stream stream)
                                   :prompt *extended-command-prompt*
                                   :prompt-mode :raw :history nil))
        (maybe-clear-input))
      (with-delimiter-gestures (*command-argument-delimiters* :override t)
        ;; FIXME, except we can't: use of CLIM-INTERNALS.
        (let* ((info (gethash command-name climi::*command-parser-table*))
               (required-args (climi::required-args info))
               (keyword-args (climi::keyword-args info)))
          (declare (ignore keyword-args))
          (let (result)
            ;; only required args for now.
            (dolist (arg required-args (cons command-name (nreverse result)))
              (destructuring-bind (name ptype &rest args) arg
                (push (esa-parse-one-arg stream name ptype args) result)
                (maybe-clear-input)))))))))

(defun esa-partial-command-parser (command-table stream command position
                                   &optional numeric-argument)
  (declare (ignore command-table position))
  (let ((command-name (car command))
	(command-args (cdr command)))
    (flet ((maybe-clear-input ()
             (let ((gesture (read-gesture :stream stream 
                                          :peek-p t :timeout 0)))
               (when (and gesture (or (delimiter-gesture-p gesture)
                                      (activation-gesture-p gesture)))
                 (read-gesture :stream stream)))))
      (with-delimiter-gestures (*command-argument-delimiters* :override t)
        ;; FIXME, except we can't: use of CLIM-INTERNALS.
        (let ((info (gethash command-name climi::*command-parser-table*)))
          (if (null info)
              ;; `command' is not a real command! Well, we can still
              ;; replace numeric argument markers.
              (substitute-numeric-argument-marker command numeric-argument)
              (let ((required-args (climi::required-args info))
                    (keyword-args (climi::keyword-args info)))
                ;; keyword arguments not yet supported
                (declare (ignore keyword-args))
                (let (result arg-parsed)
                  ;; only required args for now.
                  (do* ((required-args required-args (cdr required-args))
                        (arg (car required-args) (car required-args))
                        (command-args command-args (cdr command-args))
                        (command-arg (car command-args) (car command-args)))
                       ((null required-args) (cons command-name (nreverse result)))
                    (destructuring-bind (name ptype &rest args) arg
                      (push (cond ((eq command-arg *unsupplied-argument-marker*)
                                   (setf arg-parsed t)
                                   (esa-parse-one-arg stream name ptype args))
                                  ((eq command-arg *numeric-argument-marker*)
                                   (or numeric-argument (getf args :default)))
                                  (t (eval command-arg)))
                            result)
                      (when arg-parsed
                        (maybe-clear-input))))))))))))
