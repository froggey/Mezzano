;;;-*- LISP -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; An ITERATE driver for postgresql queries via PG (http://cliki.net/pg)
;;;           Written by Andreas Fuchs <asf@boinkor.net>
;;;
;;; Permission to use, copy, modify, and distribute this software and its
;;; documentation for any purpose and without fee is hereby granted,
;;; provided that this copyright and permission notice appear in all
;;; copies and supporting documentation, and that the name of M.I.T. not
;;; be used in advertising or publicity pertaining to distribution of the
;;; software without specific, written prior permission. M.I.T. makes no
;;; representations about the suitability of this software for any
;;; purpose.  It is provided "as is" without express or implied warranty.

;;; M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
;;; ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
;;; M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
;;; ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
;;; WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
;;; ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
;;; SOFTWARE.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Usage example:
;;; (iterate (for (impl version date) in-relation "select * from version" on-connection *dbconn*)
;;;          (collect version))

(cl:in-package :iterate)

(defvar *in-pg-transaction* nil)

(defmacro with-pg-cursor (cursor connection query &body body)
  (let ((conn (gensym))
        (begin-transaction (gensym))
        (success (gensym)))
    `(let ((,cursor (symbol-name (gensym "PGCURSOR")))
           (,conn ,connection)
           (,begin-transaction (not *in-pg-transaction*))
           (,success nil)
           (*in-pg-transaction* t))
       (when ,begin-transaction
         (pg:pg-exec ,conn "BEGIN WORK"))
       (pg:pg-exec ,conn "DECLARE " ,cursor " CURSOR FOR " ,query)
       (unwind-protect (multiple-value-prog1 (progn ,@body)
			 (setf ,success t))
         (pg:pg-exec ,conn "CLOSE " ,cursor)
         (when ,begin-transaction
           (pg:pg-exec ,conn (if ,success "COMMIT WORK" "ROLLBACK WORK")))))))

(defclause-driver (FOR var-spec IN-RELATION query ON-CONNECTION conn)
  (top-level-check)
  (let* ((row-var (make-var-and-default-binding 'row :type 'list))
         (cursor (gensym "CURSOR"))
         (test `(when (null ,row-var) (go ,*loop-end*)))
         (setq (do-dsetq var-spec row-var)))
    (add-loop-body-wrapper `(with-pg-cursor ,cursor ,conn ,query))
    (setf *loop-end-used?* t)
    (return-driver-code :next (list `(setq ,row-var (first (pg:pg-result (pg:pg-exec ,conn "FETCH 1 FROM " ,cursor) :tuples)))
                                    test
                                    setq)
                        :variable var-spec)))

;;; arch-tag: c08d68b2-63b2-4347-b261-133ae30b3e18