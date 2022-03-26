;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-FAD; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-fad/fad.lisp,v 1.35 2009/09/30 14:23:10 edi Exp $

;;; Copyright (c) 2004, Peter Seibel.  All rights reserved.
;;; Copyright (c) 2004-2010, Dr. Edmund Weitz.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-fad)

(defun component-present-p (value)
  "Helper function for DIRECTORY-PATHNAME-P which checks whether VALUE
   is neither NIL nor the keyword :UNSPECIFIC."
  (and value (not (eql value :unspecific))))

(defun directory-pathname-p (pathspec)
  "Returns NIL if PATHSPEC \(a pathname designator) does not designate
a directory, PATHSPEC otherwise.  It is irrelevant whether file or
directory designated by PATHSPEC does actually exist."
  (and
    (not (component-present-p (pathname-name pathspec)))
    (not (component-present-p (pathname-type pathspec)))
    pathspec))

(defun pathname-as-directory (pathspec)
  "Converts the non-wild pathname designator PATHSPEC to directory
form."
  (let ((pathname (pathname pathspec)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (cond ((not (directory-pathname-p pathspec))
           (make-pathname :directory (append (or (pathname-directory pathname)
                                                 (list :relative))
                                             (list (file-namestring pathname)))
                          :name nil
                          :type nil
                          :defaults pathname))
          (t pathname))))

(defun directory-wildcard (dirname)
  "Returns a wild pathname designator that designates all files within
the directory named by the non-wild pathname designator DIRNAME."
  (when (wild-pathname-p dirname)
    (error "Can only make wildcard directories from non-wildcard directories."))
  (make-pathname :name #-:cormanlisp :wild #+:cormanlisp "*"
                 :type #-(or :clisp :cormanlisp) :wild
                       #+:clisp nil
                       #+:cormanlisp "*"
                 :defaults (pathname-as-directory dirname)))

#+:clisp
(defun clisp-subdirectories-wildcard (wildcard)
  "Creates a wild pathname specifically for CLISP such that
sub-directories are returned by DIRECTORY."
  (make-pathname :directory (append (pathname-directory wildcard)
                                    (list :wild))
                 :name nil
                 :type nil
                 :defaults wildcard))

(defun list-directory (dirname &key (follow-symlinks t))
  "Returns a fresh list of pathnames corresponding to all files within
   the directory named by the non-wild pathname designator DIRNAME.
   The pathnames of sub-directories are returned in directory form -
   see PATHNAME-AS-DIRECTORY.

  If FOLLOW-SYMLINKS is true, then the returned list contains
truenames (symlinks will be resolved) which essentially means that it
might also return files from *outside* the directory.  This works on
all platforms.

  When FOLLOW-SYMLINKS is NIL, it should return the actual directory
contents, which might include symlinks.  Currently this works on SBCL
and CCL."
  (declare (ignorable follow-symlinks))
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  #+(or :ecl :clasp)
  (let ((dir (pathname-as-directory dirname)))
    (concatenate 'list
                 (directory (merge-pathnames (pathname "*/") dir))
                 (directory (merge-pathnames (pathname "*.*") dir))))
  #-(or :ecl :clasp)
  (let ((wildcard (directory-wildcard dirname)))
    #+:abcl (system::list-directory dirname)
    #+:sbcl (directory wildcard :resolve-symlinks follow-symlinks)
    #+(or :cmu :scl :lispworks :mezzano) (directory wildcard)
    #+(or :openmcl :digitool) (directory wildcard :directories t :follow-links follow-symlinks)
    #+:allegro (directory wildcard :directories-are-files nil)
    #+:clisp (nconc (directory wildcard :if-does-not-exist :keep)
                    (directory (clisp-subdirectories-wildcard wildcard)))
    #+:cormanlisp (nconc (directory wildcard)
                         (cl::directory-subdirs dirname)))
  #-(or :sbcl :cmu :scl :lispworks :openmcl :allegro :clisp :cormanlisp :ecl :abcl :digitool :clasp :mezzano)
  (error "LIST-DIRECTORY not implemented"))

(defun pathname-as-file (pathspec)
  "Converts the non-wild pathname designator PATHSPEC to file form."
  (let ((pathname (pathname pathspec)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (cond ((directory-pathname-p pathspec)
           (let* ((directory (pathname-directory pathname))
                  (name-and-type (pathname (first (last directory)))))
             (make-pathname :directory (butlast directory)
                            :name (pathname-name name-and-type)
                            :type (pathname-type name-and-type)
                            :defaults pathname)))
          (t pathname))))

(defun file-exists-p (pathspec)
  "Checks whether the file named by the pathname designator PATHSPEC
exists and returns its truename if this is the case, NIL otherwise.
The truename is returned in `canonical' form, i.e. the truename of a
directory is returned as if by PATHNAME-AS-DIRECTORY."
  #+(or :sbcl :lispworks :openmcl :ecl :digitool clasp) (probe-file pathspec)
  #+:allegro (or (excl:probe-directory (pathname-as-directory pathspec))
                 (probe-file pathspec))
  #+(or :cmu :scl :abcl) (or (probe-file (pathname-as-directory pathspec))
                             (probe-file pathspec))
  #+:cormanlisp (or (and (ccl:directory-p pathspec)
                         (pathname-as-directory pathspec))
                    (probe-file pathspec))
  #+:clisp (or (ignore-errors
                 (let ((directory-form (pathname-as-directory pathspec)))
                   (when (ext:probe-directory directory-form)
                     (truename directory-form))))
               (ignore-errors
                 (probe-file (pathname-as-file pathspec))))
  #-(or :sbcl :cmu :scl :lispworks :openmcl :allegro :clisp :cormanlisp :ecl :abcl :digitool :clasp)
  (error "FILE-EXISTS-P not implemented"))

(defun directory-exists-p (pathspec)
  "Checks whether the file named by the pathname designator PATHSPEC
exists and if it is a directory.  Returns its truename if this is the
case, NIL otherwise.  The truename is returned in directory form as if
by PATHNAME-AS-DIRECTORY."
  #+:allegro
  (and (excl:probe-directory pathspec)
       (pathname-as-directory (truename pathspec)))
  #+:lispworks
  (and (lw:file-directory-p pathspec)
       (pathname-as-directory (truename pathspec)))
  #-(or :allegro :lispworks)
  (let ((result (file-exists-p pathspec)))
    (and result
         (directory-pathname-p result)
         result)))

(defun walk-directory (dirname fn &key directories
                                       (if-does-not-exist :error)
                                       (test (constantly t))
                                       (follow-symlinks t))
  "Recursively applies the function FN to all files within the
directory named by the non-wild pathname designator DIRNAME and all of
its sub-directories.  FN will only be applied to files for which the
function TEST returns a true value.  If DIRECTORIES is not NIL, FN and
TEST are applied to directories as well.  If DIRECTORIES
is :DEPTH-FIRST, FN will be applied to the directory's contents first.
If DIRECTORIES is :BREADTH-FIRST and TEST returns NIL, the directory's
content will be skipped. IF-DOES-NOT-EXIST must be one of :ERROR
or :IGNORE where :ERROR means that an error will be signaled if the
directory DIRNAME does not exist.  If FOLLOW-SYMLINKS is T, then your
callback will receive truenames.  Otherwise you should get the actual
directory contents, which might include symlinks.  This might not be
supported on all platforms.  See LIST-DIRECTORY."
  (labels ((walk (name)
             (cond
               ((directory-pathname-p name)
                ;; the code is written in a slightly awkward way for
                ;; backward compatibility
                (cond ((not directories)
                       (dolist (file (list-directory name :follow-symlinks follow-symlinks))
                         (walk file)))
                      ((eql directories :breadth-first)
                       (when (funcall test name)
                         (funcall fn name)
                         (dolist (file (list-directory name :follow-symlinks follow-symlinks))
                           (walk file))))
                      ;; :DEPTH-FIRST is implicit
                      (t (dolist (file (list-directory name :follow-symlinks follow-symlinks))
                           (walk file))
                         (when (funcall test name)
                           (funcall fn name)))))
               ((funcall test name)
                (funcall fn name)))))
    (let ((pathname-as-directory (pathname-as-directory dirname)))
      (case if-does-not-exist
        ((:error)
         (cond ((not (file-exists-p pathname-as-directory))
                (error "File ~S does not exist."
                       pathname-as-directory))
               (t (walk pathname-as-directory))))
        ((:ignore)
         (when (file-exists-p pathname-as-directory)
           (walk pathname-as-directory)))
        (otherwise
         (error "IF-DOES-NOT-EXIST must be one of :ERROR or :IGNORE."))))
    (values)))

(defvar *stream-buffer-size* 8192)

(defun copy-stream (from to &optional (checkp t))
  "Copies into TO \(a stream) from FROM \(also a stream) until the end
of FROM is reached, in blocks of *stream-buffer-size*.  The streams
should have the same element type.  If CHECKP is true, the streams are
checked for compatibility of their types."
  (when checkp
    (unless (subtypep (stream-element-type to) (stream-element-type from))
      (error "Incompatible streams ~A and ~A." from to)))
  (let ((buf (make-array *stream-buffer-size*
                         :element-type (stream-element-type from))))
    (loop
       (let ((pos #-:clisp (read-sequence buf from)
                  #+:clisp (ext:read-byte-sequence buf from :no-hang nil)))
         (when (zerop pos) (return))
         (write-sequence buf to :end pos))))
  (values))

(defun copy-file (from to &key overwrite)
  "Copies the file designated by the non-wild pathname designator FROM
to the file designated by the non-wild pathname designator TO.  If
OVERWRITE is true overwrites the file designated by TO if it exists."
  #+:allegro (excl.osi:copy-file from to :overwrite overwrite)
  #-:allegro
  (let ((element-type #-:cormanlisp '(unsigned-byte 8)
                      #+:cormanlisp 'unsigned-byte))
    (with-open-file (in from :element-type element-type)
      (with-open-file (out to :element-type element-type
                              :direction :output
                              :if-exists (if overwrite
                                           :supersede
                                           #-:cormanlisp :error
                                           #+:cormanlisp nil))
        #+:cormanlisp
        (unless out
          (error (make-condition 'file-error
                                 :pathname to
                                 :format-control "File already exists.")))
        (copy-stream in out))))
  (values))

(defun delete-directory-and-files (dirname &key (if-does-not-exist :error))
  "Recursively deletes all files and directories within the directory
designated by the non-wild pathname designator DIRNAME including
DIRNAME itself.  IF-DOES-NOT-EXIST must be one of :ERROR or :IGNORE
where :ERROR means that an error will be signaled if the directory
DIRNAME does not exist.

NOTE: this function is dangerous if the directory that you are
removing contains symlinks to files outside of it - the target files
might be removed instead!  This is currently fixed for SBCL and CCL."

  #+:allegro (excl.osi:delete-directory-and-files dirname
                                                  :if-does-not-exist if-does-not-exist)

  #+:sbcl
  (if (directory-exists-p dirname)
      (sb-ext:delete-directory dirname :recursive t)
      (ecase if-does-not-exist
        (:error  (error "~S is not a directory" dirname))
        (:ignore nil)))

  #+:ccl-has-delete-directory
  (if (directory-exists-p dirname)
      (ccl:delete-directory dirname)
      (ecase if-does-not-exist
        (:error  (error "~S is not a directory" dirname))
        (:ignore nil)))

  #-(or :allegro :sbcl :ccl-has-delete-directory)
  (walk-directory dirname
                  (lambda (file)
                    (cond ((directory-pathname-p file)
                           #+:lispworks (lw:delete-directory file)
                           #+:cmu (multiple-value-bind (ok err-number)
                                      (unix:unix-rmdir (namestring (truename file)))
                                    (unless ok
                                      (error "Error number ~A when trying to delete ~A"
                                             err-number file)))
                           #+:scl (multiple-value-bind (ok errno)
                                      (unix:unix-rmdir (ext:unix-namestring (truename file)))
                                    (unless ok
                                      (error "~@<Error deleting ~S: ~A~@:>"
                                             file (unix:get-unix-error-msg errno))))
                           #+:clisp (ext:delete-dir file)
                           #+:openmcl (cl-fad-ccl:delete-directory file)
                           #+:cormanlisp (win32:delete-directory file)
                           #+:ecl (si:rmdir file)
                           #+:clasp (core:rmdir file)
                           #+(or :abcl :digitool) (delete-file file))
                          (t (delete-file file))))
                  :follow-symlinks nil
                  :directories t
                  :if-does-not-exist if-does-not-exist)
  (values))

(defun pathname-directory-pathname (pathname)
  "Returns a complete pathname representing the directory of
PATHNAME. If PATHNAME is already a directory pathname (name NIL, type
NIL) returns a pathname equal (as per pathname=) to it."
  (make-pathname :defaults pathname
                 :name nil :type nil))

(defun pathname-parent-directory (pathname)
  "Returns a pathname which would, by name at least, contain PATHNAME
as one of its direct children. Symlinks can make the parent/child
relationship a like opaque, but generally speaking the value returned
by this function is a directory name which contains PATHNAME.

The root directory, #P\"/\", is its own parent. The parent directory
of a filename is the parent of the filename's dirname."
  (canonical-pathname
   (make-pathname :defaults pathname
                  :directory (if (pathname-root-p pathname)
                                 (list :absolute)
                                 (append (or (pathname-directory pathname)
                                             (list :relative))
                                         (list :back))))))

(defun canonical-pathname (pathname)
  "Remove redundant information from PATHNAME.

This simply walks down PATHNAME's pathname-directory and drops \".\"
directories, removes :back and its preceding element.

NB: This function does not access the filesystem, it only looks at the
values in the pathname and works on their known (or assumed)
meanings.

NB: Since this function does not access the filesystem it will only
remove :BACK elements from the path (not :UP elements). Since some
lisps, ccl/sbcl/clisp convert \"..\" in pathnames to :UP, and
not :BACK, the actual utility of the function is limited."
  (let ((pathname (pathname pathname))) ;; just make sure to get a pathname object
    (loop
       with full-dir = (or (pathname-directory pathname)
                           (list :relative))
       with canon-dir = (if (member (first full-dir) '(:relative :absolute))
                            (list (pop full-dir))
                            (list :relative))
       while full-dir
       do (cond
            ((string= "." (first full-dir))
             (pop full-dir))
            ((eql :back (second full-dir))
             (pop full-dir)
             (pop full-dir))
            (t (push (pop full-dir) canon-dir)))
       finally (return (make-pathname :defaults pathname :directory (nreverse canon-dir))))))

(defun merge-pathnames-as-directory (&rest pathnames)
  "Given a list of (probably relative) pathnames, this returns a single
directory pathname containing the logical concatenation of them all.

The returned value is the current directory if one were to cd into
each of PATHNAMES in order. For this reason an absolute pathname will,
effectively, cancel the affect of any previous relative pathnames.

The returned value's defaults are taken from the first element of
PATHNAMES (host, version and device).

NB: Since this function only looks at directory names the name and
type of the elements of PATHNAMES are ignored. Make sure to properly
use either trailing #\\/s, or pathname-as-directory, to get the
expected results.

Examples:

    (merge-pathnames-as-directory #P\"foo/\" #P\"bar/\") == #P\"foo/bar/\"
    (merge-pathnames-as-directory #P\"foo/\" #P\"./bar/\") == #P\"foo/./bar/\"
    (merge-pathnames-as-directory #P\"foo/\" #P\"/bar/\") == #P\"/bar/\"
    (merge-pathnames-as-directory #P\"foo/\" #P\"/bar/\" #P\"quux/file.txt\") == #P\"/bar/quux/\"
"
  (if pathnames
      (let* ((pathnames (mapcar #'pathname pathnames))
             (defaults (first pathnames))
             (dir (pathname-directory defaults)))
        (loop for pathname in (rest pathnames)
              for directory = (pathname-directory pathname)
              do (ecase (first directory)
                   ;; this is equivalent to (:relative) == ".", so,
                   ;; for this function, just do nothing.
                   ((nil)) 
                   (:absolute
                    (setf dir directory))
                   (:relative
                    (setf dir (append dir (rest directory))))))
        (make-pathname :defaults defaults :directory dir :name nil :type nil))
      (make-pathname)))

(defun merge-pathnames-as-file (&rest pathnames)
    "Given a list of, probably relative, pathnames returns a single
filename pathname containing the logical concatenation of them all.

The returned value's defaults are taken from the first element of
PATHNAMES (host, version and device). The returned values's name, type
and version are taken from the last element of PATHNAMES. The
intervening elements are used only for their pathname-directory
values.

Examples:

    (merge-pathnames-as-file #P\"foo/\" #P\"bar.txt\") == #P\"foo/bar.txt\"
    (merge-pathnames-as-file #P\"foo/\" #P\"./bar.txt\") == #P\"foo/./bar.txt\"
    (merge-pathnames-as-file #P\"foo/\" #P\"/bar/README\") == #P\"/bar/README\"
    (merge-pathnames-as-file #P\"/foo/\" #P\"/bar/\" #P\"quux/file.txt\") == #P\"/bar/quux/file.txt\"
"
    (cond ((null pathnames)
           (make-pathname))
          ((null (cdr pathnames))
           (pathname-as-file (first pathnames)))
          (t
           (let ((file-name-part (first (last pathnames))))
             (make-pathname :defaults (apply #'merge-pathnames-as-directory pathnames)
                            :name (pathname-name file-name-part)
                            :type (pathname-type file-name-part)
                            :version (pathname-version file-name-part))))))

(defmacro with-component-testers ((a b key) &body body)
  (let ((k (gensym)))
    `(let* ((,k ,key)
            (,a (funcall ,k ,a))
            (,b (funcall ,k ,b)))
       (labels ((components-are (test)
                  (and (funcall test ,a) (funcall test ,b)))
                (components-are-member (values)
                  (and (member ,a values :test #'eql)
                       (member ,b values :test #'eql)
                       (eql ,a ,b)))
                (components-are-string= ()
                  (and (stringp ,a) (stringp ,b) (string= ,a ,b)))
                (components-are-every (test)
                  (and (consp ,a)
                       (consp ,b)
                       (every test ,a ,b))))
         (declare (ignorable #'components-are #'components-are-member
                             #'components-are-string= #'components-are-every))
         (if (or ,@body)
             (values t ,a ,b)
             nil)))))

(defun pathname-host-equal (a b)
  (with-component-testers (a b #'pathname-host)
    (eq a b)
    (components-are-member '(nil :unspecific))
    (components-are-string=)
    (components-are-every #'string=)))

(defun pathname-device-equal (a b)
  (with-component-testers (a b #'pathname-device)
    (components-are-member '(nil :unspecific))
    (components-are-string=)))

(defun pathname-directory-equal (a b)
  (with-component-testers (a b #'pathname-directory)
    (and (null a) (null b))
    (and (= (length a) (length b))
         (every (lambda (a b)
                  (or (and (stringp a) (stringp b) (string= a b))
                      (and (null a) (null b))
                      (and (keywordp a) (keywordp b) (eql a b))))
                a b))))

(defun pathname-name-equal (a b)
  (with-component-testers (a b #'pathname-name)
    (components-are-member '(nil :wild :unspecific))
    (components-are-string=)))

(defun pathname-type-equal (a b)
  (with-component-testers (a b #'pathname-type)
    (components-are-member '(nil :wild :unspecific))
    (components-are-string=)))

(defun pathname-version-equal (a b)
  (with-component-testers (a b #'pathname-version)
    (and (null a) (null b))
    (components-are-member '(:wild :newest :unspecific))
    (and (integerp a) (integerp b) (= a b))))

(defun pathname-equal (a b)
  "Returns T if A and B represent the same pathname. This function
does not access the filesystem, it only looks at the components of the
two pathnames to test if they are the same (though by
passing both A and B to probe-file one can make this function test for file 'sameness'.

Equality is defined as:

  - strings that are string equal
  - symbol (including nil) or keywords which are eql
  - lists of the same length with equal (as per these rules) elements.

if any of these tree conditions is false for any of the components in
A and B then A and B are different, otherwise they are the same.

NB: This function does not convert name strings to pathnames. So
\"foo.txt\" and #P\"foo.txt\" are different pathnames."
  (if (and a b)
      (if (and (pathname-host-equal a b)
               (pathname-device-equal a b)
               (pathname-directory-equal a b)
               (pathname-name-equal a b)
               (pathname-type-equal a b)
               (pathname-version-equal a b))
          (values t a b)
          (values nil))
      (values nil)))

(defun pathname-absolute-p (a)
  "Returns true if A is an absolute pathname.

This simply tests if A's directory list starts with :ABSOLUTE"
  (eql :absolute (first (pathname-directory (pathname a)))))

(defun pathname-relative-p (a)
  "Returns true if A is a relative pathname.

This simply tests if A's directory starts with :RELATIVE."
  (let ((dir (pathname-directory (pathname a))))
    (or (null dir) (eql :relative (first dir)))))

(defun pathname-root-p (a)
  (let ((dir (pathname-directory (pathname a))))
    (and (eql :absolute (first dir))
         (= 1 (length dir)))))

(pushnew :cl-fad *features*)

;; stuff for Nikodemus Siivola's HYPERDOC
;; see <http://common-lisp.net/project/hyperdoc/>
;; and <http://www.cliki.net/hyperdoc>
;; also used by LW-ADD-ONS

#-:abcl
(defvar *hyperdoc-base-uri* "http://weitz.de/cl-fad/")

#-:abcl
(let ((exported-symbols-alist
       (loop for symbol being the external-symbols of :cl-fad
             collect (cons symbol
                           (concatenate 'string
                                        "#"
                                        (string-downcase symbol))))))
  (defun hyperdoc-lookup (symbol type)
    (declare (ignore type))
    (cdr (assoc symbol
                exported-symbols-alist
                :test #'eq))))
