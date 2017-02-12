;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;; Function for parsing various parts of CL.

(in-package :sys.int)

(defun parse-ordinary-lambda-list (lambda-list)
  "Parse LAMBDA-LIST as an ordinary lambda list.
Returns the (required optional rest enable-keys keys allow-other-keys aux fref closure) elements
from the lambda-list or throws an error if the lambda-list is invalid."
  (let ((state :required)
        (required '())
        (optional '())
        (rest nil)
        (enable-keys nil)
        (keys '())
        (allow-other-keys nil)
        (aux '())
        (fref nil)
        (closure nil)
        (count nil))
    (dolist (i lambda-list)
      (ecase state
        (:required (case i
                     (&allow-other-keys
                      (error "Unexpected ~S in lambda list ~S" i lambda-list))
                     (&optional (setf state :optional))
                     (&rest (setf state :rest))
                     (&key (setf state :key
                                 enable-keys t))
                     (&aux (setf state :aux))
                     (&fref (setf state :fref))
                     (&closure (setf state :closure))
                     (&count (setf state :count))
                     (t (when (not (symbolp i))
                          (error "Required parameters must be symbols"))
                        (push i required))))
        (:optional (case i
                     ((&optional &allow-other-keys)
                      (error "Unexpected ~S in lambda list ~S" i lambda-list))
                     (&rest (setf state :rest))
                     (&key (setf state :key
                                 enable-keys t))
                     (&aux (setf state :aux))
                     (&fref (setf state :fref))
                     (&closure (setf state :closure))
                     (&count (setf state :count))
                     (t (etypecase i
                          (symbol (push `(,i nil nil) optional))
                          (cons (let ((name (car i))
                                      (default nil)
                                      (supplied-p nil))
                                  (unless (symbolp name)
                                    (error "Invalid optional parameter ~S in lambda list ~S" i lambda-list))
                                  (when (cdr i)
                                    (unless (consp (cdr i))
                                      (error "Invalid optional parameter ~S in lambda list ~S" i lambda-list))
                                    (setf default (cadr i))
                                    (when (cddr i)
                                      (when (or (not (consp (cddr i)))
                                                (cdddr i)
                                                (null (caddr i))
                                                (not (symbolp (caddr i))))
                                        (error "Invalid optional parameter ~S in lambda list ~S" i lambda-list))
                                      (setf supplied-p (caddr i))))
                                  (push (list name default supplied-p) optional)))))))
        (:rest (case i
                 ((&optional &rest &key &allow-other-keys &aux &fref &closure &count)
                  (error "Unexpected ~S in lambda list ~S" i lambda-list))
                 ((nil) (error "Invalid &REST parameter name"))
                 (t (setf state :after-rest
                          rest i))))
        (:after-rest (case i
                       (&key (setf state :key
                                   enable-keys t))
                       (&aux (setf state :aux))
                       (&fref (setf state :fref))
                       (&closure (setf state :closure))
                       (&count (setf state :count))
                       (t (error "Unexpected ~S in lambda list ~S" i lambda-list))))
        (:key (case i
                ((&optional &rest &key)
                 (error "Unexpected ~S in lambda list ~S" i lambda-list))
                (&allow-other-keys
                 (setf state :after-allow-other-keys
                       allow-other-keys t))
                (&aux (setf state :aux))
                (&fref (setf state :fref))
                (&closure (setf state :closure))
                (&count (setf state :count))
                (t (let (keyword name default supplied-p)
                     (etypecase i
                       (symbol (setf name i
                                     keyword (intern (symbol-name i) "KEYWORD")))
                       (cons
                        (if (consp (car i))
                            (if (or (not (symbolp (caar i)))
                                    (not (consp (cdar i)))
                                    (cddar i)
                                    (not (symbolp (cadar i))))
                                (error "Invalid key parameter ~S in lambda list ~S" i lambda-list)
                                (setf name (cadar i)
                                      keyword (caar i)))
                            (if (not (symbolp (car i)))
                                (error "Invalid key parameter ~S in lambda list ~S" i lambda-list)
                                (setf name (car i)
                                      keyword (intern (symbol-name (car i)) "KEYWORD"))))
                        (when (cdr i)
                          (unless (consp (cdr i))
                            (error "Invalid key parameter ~S in lambda list ~S" i lambda-list))
                          (setf default (cadr i))
                          (when (cddr i)
                            (setf supplied-p (caddr i))
                            (when (or (not (consp (cddr i)))
                                      (cdddr i)
                                      (null (caddr i))
                                      (not (symbolp (caddr i))))
                              (error "Invalid key parameter ~S in lambda list ~S" i lambda-list))))))
                     (push (list (list keyword name) default supplied-p) keys)))))
        (:after-allow-other-keys
         (case i
           (&aux (setf state :aux))
           (&fref (setf state :fref))
           (&closure (setf state :closure))
           (&count (setf state :count))
           (t (error "Unexpected ~S in lambda list ~S" i lambda-list))))
        (:aux (case i
                ((&optional &rest &key &allow-other-keys &aux)
                 (error "Unexpected ~S in lambda list ~S" i lambda-list))
                (&fref (setf state :fref))
                (&closure (setf state :closure))
                (&count (setf state :count))
                (t (let (name default)
                     (etypecase i
                       (symbol (setf name i))
                       (cons
                        (unless (symbolp (car i))
                          (error "Invalid aux parameter ~S in lambda list ~S" i lambda-list))
                        (setf name (car i))
                        (when (cdr i)
                          (when (or (not (consp (cdr i))) (cddr i))
                            (error "Invalid aux parameter ~S in lambda list ~S" i lambda-list))
                          (setf default (cadr i)))))
                     (push (list name default) aux)))))
        (:fref (case i
                 ((&optional &rest &key &allow-other-keys &aux &fref &closure &count)
                  (error "Unexpected ~S in lambda list ~S" i lambda-list))
                 ((nil) (error "Invalid &FREF parameter name"))
                 (t (setf state :after-fref
                          fref i))))
        (:after-fref (case i
                       (&closure (setf state :closure))
                       (&count (setf state :count))
                       (t (error "Unexpected ~S in lambda list ~S" i lambda-list))))
        (:closure (case i
                    ((&optional &rest &key &allow-other-keys &aux &fref &closure &count)
                     (error "Unexpected ~S in lambda list ~S" i lambda-list))
                    ((nil) (error "Invalid &CLOSURE parameter name"))
                    (t (setf state :after-closure
                             closure i))))
        (:after-closure (case i
                          (&count (setf state :count))
                          (t (error "Unexpected ~S in lambda list ~S" i lambda-list))))
        (:count (case i
                 ((&optional &rest &key &allow-other-keys &aux &fref &closure &count)
                  (error "Unexpected ~S in lambda list ~S" i lambda-list))
                 ((nil) (error "Invalid &COUNT parameter name"))
                 (t (setf state :after-count
                          count i))))
        (:after-count
         (error "Unexpected ~S in lambda list ~S" i lambda-list))))
    (when (eql state :rest)
      (error "Missing &REST parameter name after &REST in lambda list ~S" lambda-list))
    (when (eql state :fref)
      (error "Missing &FREF parameter name after &FREF in lambda list ~S" lambda-list))
    (when (eql state :closure)
      (error "Missing &CLOSURE parameter name after &CLOSURE in lambda list ~S" lambda-list))
    (when (eql state :count)
      (error "Missing &COUNT parameter name after &COUNT in lambda list ~S" lambda-list))
    (values (nreverse required) (nreverse optional) rest enable-keys (nreverse keys) allow-other-keys (nreverse aux) fref closure count)))

(defun parse-declares (forms &key permit-docstring)
  "Extract any leading declare forms and an optional docstring from FORMS
Returns 3 values:
The body, with the declare forms and the docstring removed.
A list of any declaration-specifiers.
The docstring, if permitted and present; otherwise NIL."
  (do ((declares '())
       (docstring nil)
       (itr forms (cdr itr)))
      ((or (null itr)
           ;; A string at the end of forms must always be treated
           ;; as a body form, not a docstring.
           ;; A string seen when permit-docstring is false also
           ;; finishes parsing.
           (and (stringp (car itr)) (or (not permit-docstring)
                                        (null (cdr itr))))
           ;; Stop when (car itr) is not a string and is not a declare form.
           (not (or (stringp (car itr))
                    (and (consp (car itr))
                         (eq 'declare (caar itr))))))
       (values itr (nreverse declares) docstring))
    (if (stringp (car itr))
        (unless docstring
          (setf docstring (car itr)))
        ;; Dump the bodies of each declare form into a single list.
        (dolist (decl (cdar itr))
          (push decl declares)))))

(defun parse-let-binding (binding)
  (etypecase binding
    (symbol (values binding nil))
    (cons (destructuring-bind (name &optional init-form)
              binding
            (check-type name symbol)
            (values name init-form)))))

(defun parse-flet-binding (binding)
  (destructuring-bind (name lambda-list &body body)
      binding
    (values name lambda-list body)))

(defun parse-eval-when-situation (situation)
  (let (compile load eval)
    (dolist (s situation)
      (ecase s
        ((:compile-toplevel compile) (setf compile t))
        ((:load-toplevel load) (setf load t))
        ((:execute eval) (setf eval t))))
    (values compile load eval)))

(defun parse-lambda (form)
  "Parse a lambda expression, returning its name, lambda-list and body forms."
  (unless (eq (car form) 'lambda)
    (error "Lambda expression must start with LAMBDA."))
  (when (null (cdr form))
       (error "Lambda expression ~S has no lambda-list." form))
  (values (cadr form) (cddr form)))

(defun lambda-expression-p (form)
  "Test if FORM is a lambda expression (a list starting with the symbol LAMBDA)."
  (and (consp form) (eq (car form) 'lambda)))
