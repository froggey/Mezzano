(cl:in-package #:clim-tests)

(def-suite* :mcclim.utils
  :in :mcclim)

(test remove-duplicated-points
  (is (equal nil (climi::remove-duplicated-points nil)))
  (let ((p1 (make-point 1 1))
        (p2 (make-point 2 2)))
    (is (equal (list p1 p2)    (climi::remove-duplicated-points (list p1 p2))))
    (is (equal (list p1 p2)    (climi::remove-duplicated-points (list p1 p2 p2))))
    (is (equal (list p1 p2)    (climi::remove-duplicated-points (list p1 p2 p2 p2))))
    (is (equal (list p1 p2 p1) (climi::remove-duplicated-points (list p1 p2 p2 p1))))
    (is (equal (list p1 p2 p1) (climi::remove-duplicated-points (list p1 p2 p2 p1 p1))))
    (is (equal (list p1 p2)    (climi::remove-duplicated-points (list p1 p2 p2 p1 p1) t)))))


;;; Line splitting utility
;;; ============================================================================
(defun list-lines (string start end breaks)
  (declare (ignore end))
  (climi::collect (line)
    (do ((start start (car breaks))
         (breaks breaks (rest breaks)))
        ((null breaks)
         (line))
      (line (subseq string start (car breaks))))))

(defun lines (string &rest args &key (start 0) end margin (offset 0) count break-strategy)
  (declare (ignore margin count break-strategy))
  (list-lines string start end
              (apply #'climi::line-breaks string 1 :start start :initial-offset offset
                     (alexandria:remove-from-plist args :start :offset))))

(test line-breaks.smoke
  ;; Does not work on empty strings.
  (signals error (lines ""))
  ;; Some cases.
  (let* ((string "ala ma kota a kot ma alę")
         (prefix "XXX XXX ")
         (suffix "XXX XXX ")
         (string1 (concatenate 'string prefix string suffix))
         (string2 (concatenate 'string prefix string))
         (string3 (concatenate 'string        string suffix))
         ;;
         (start1 (length prefix))
         (start2 start1)
         (end1 (- (length string1) (length suffix)))
         (end3 (- (length string3) (length suffix))))
    (flet ((check (margin offset result)
             (is (equal result (lines string  :margin margin :offset offset                        )))
             (is (equal result (lines string1 :margin margin :offset offset :start start1 :end end1)))
             (is (equal result (lines string2 :margin margin :offset offset :start start2          )))
             (is (equal result (lines string3 :margin margin :offset offset               :end end3)))))
      ;; Test emergency breaks with negative initial offset.
      (check 0 -3 '("ala" " " "m" "a" " " "k" "o" "t" "a" " " "a" " " "k" "o" "t" " " "m" "a" " " "a" "l" "ę"))
      ;; Similar to above but does not trigger a degenarte case where we violate the layout.
      (check 1 -2 '("ala" " " "m" "a" " " "k" "o" "t" "a" " " "a" " " "k" "o" "t" " " "m" "a" " " "a" "l" "ę"))
      ;; Usual case where each word may fit in a line.
      (check 5  1 '("ala " "ma " "kota " "a " "kot " "ma " "alę")))))

(defun gen-word ()
  (gen-one-element "lorem" "ipsum"))

(defun gen-text-element ()
  (let ((word (gen-word)))
    (lambda ()
      (case (random 10)
        (9 #\Newline)
        (t (funcall word))))))

(defun gen-text (&key (length (gen-integer :min 1 :max 20))
                      (element (gen-text-element)))
  (let ((elements (gen-list :length length :elements element)))
    (lambda ()
      (with-output-to-string (stream)
        (loop for previous = nil then element
              for element in (funcall elements)
              do (cond ((and previous (not (eql previous #\Newline)) (not (eql element #\Newline)))
                        (write-char #\Space stream)
                        (write-string element stream))
                       (t ; (and (eql previous #\Newline) (not (eql element #\Newline)))
                        (princ element stream))))))))

(defun gen-offset (&key (integer (gen-integer :min -10 :max 10)))
  (lambda ()
    (case (random 10)
      ((8 9 10) nil)
      (t        (funcall integer)))))

(defun gen-margin (&key (integer (gen-integer :min 1 :max 10)))
  (lambda ()
    (case (random 10)
      ((8 9 10) nil)
      (t        (funcall integer)))))

(defun gen-count (&key (integer (gen-integer :min 1 :max 10)))
  (lambda ()
    (case (random 10)
      ((8 9 10) nil)
      (t        (funcall integer)))))

(test line-breaks.random
  (for-all ((text     (gen-text))
            (offset   (gen-offset))
            (margin   (gen-margin))
            (count    (gen-count))
            (strategy (gen-one-element nil t)))

    (block nil
      (let* ((lines (handler-case
                        (apply #'lines text
                               :break-strategy strategy
                               (append (when offset (list :offset offset))
                                       (when margin (list :margin margin))
                                       (when count (list :count count))))
                      (error (condition)
                        (fail "For input~@
                               ~2@T~S~@
                               ~S ~S ~S ~S ~S ~S ~S ~S, signaled ~A."
                              text :offset offset :margin margin :count count :break-strategy strategy
                              condition)
                        (return))))
             (string (apply #'concatenate 'string lines)))
        (is-false (null lines)
                  "For input~@
                   ~2@T~S~@
                   ~S ~S ~S ~S ~S ~S ~S ~S, no lines were returned."
                  text :offset offset :margin margin :count count :break-strategy strategy)
        (if count
            (is-true (alexandria:starts-with-subseq string text))
            (is (string= text string)))
        (loop for line in lines
              do (is (null (find #\Newline (string-right-trim '(#\newline #\Linefeed #\return) line)))
                     "For input~@
                      ~2@T~S~@
                      ~S ~S ~S ~S ~S ~S ~S ~S, breaking into~@
                      ~2@T~S~@
                      contains a hard newline in ~S."
                     text :offset offset :margin margin :count count :break-strategy strategy
                     lines line))))))

;;; for interactive testing
(defun print-lines (string start margin &optional (offset 0) bstr
                                        &aux (pre (if (>= offset 0)
                                                      ""
                                                      (make-string (abs offset) :initial-element #\space))))
  (princ pre)
  (princ "|")
  (dotimes (i margin)
    (princ "-"))
  (princ "|")
  (let ((lines (list-lines string start (length string)
                           (climi::line-breaks string 1 :margin margin
                                                        :initial-offset offset
                                                        :start start
                                                        :break-strategy bstr)))
        (raggedness 0)
        (last-score 0))
    (format t "~16tLine width: ~s" margin)
    (flet ((pline (line off margin &aux (counter 0))
             (dotimes (i off) (princ #\space))
             (princ line)
             (dotimes (i (- margin (length line) off))
               (incf counter)
               (princ #\_))
             (princ "|")
             (format t "~16tRemaining space: ~s" counter)
             (setf last-score (expt counter 2))
             (incf raggedness last-score)))
      (terpri)
      (princ "|")
      (pline (first lines) offset margin)
      (dolist (line (rest lines))
        (terpri)
        (princ pre)
        (princ "|")
        (pline line 0 margin))
      (princ " (ignored)")
      (terpri)
      (princ pre)
      (princ "|")
      (dotimes (i margin)
        (princ "-"))
      (princ "|")
      (decf raggedness last-score)
      (format t "~16tLines: ~s; Raggedness: ~s"  (length lines) raggedness))))


(defun gen-unit ()
  (gen-one-element :character :line :point :pixel :mm))

(defun gen-invalid-space-spec ()
  (let ((value (gen-integer :min 0 :max 100))
        (unit (gen-unit)))
    (lambda ()
      (case (random 5)
        (0 :invalid-space)
        (1 #C(1 2))
        (2 (list (funcall value) :invalid-unit))
        (3 (list :invalid-real (funcall unit)))
        (4 (list (funcall value) (funcall unit) 3))))))

(defun gen-invalid-margin-spec-element ()
  (let ((anchor (gen-one-element :relative :absolute))
        (space (gen-invalid-space-spec)))
    (lambda ()
      (case (random 3)
        (0 :foo)
        (1 (list (funcall anchor) 1 2))
        (2 (list (funcall anchor) (funcall space)))))))

(defun gen-invalid-margin-spec ()
  (let ((edge (gen-one-element :left :right :top :bottom))
        (element (gen-invalid-margin-spec-element)))
    (lambda ()
      (list (funcall edge) (funcall element)))))

(test normalize-margin-spec
  (flet ((equals (spec-1 spec-2)
           (and (= (length spec-1) (length spec-2))
                (loop for (edge value) on spec-1 by #'cddr
                      always (equal value (getf spec-2 edge))))))
    (is (equals '(:left (:relative 0)
                  :top (:relative 0)
                  :right (:relative 0)
                  :bottom (:relative 0))
                (climi::normalize-margin-spec nil
                                              '(:left (:relative 0)
                                                :top (:relative 0)
                                                :right (:relative 0)
                                                :bottom (:relative 0)))))
    (is (equals '(:left (:relative 42)
                  :top (:relative 42)
                  :right (:absolute 3)
                  :bottom (:relative 0))
                (climi::normalize-margin-spec '(:left 42
                                                :top 42
                                                :right (:absolute 3)
                                                :bottom 0)
                                              '(:left (:relative 1)
                                                :top (:relative 1)
                                                :right (:relative 1)
                                                :bottom (:relative 1)))))
    (is (equals '(:left (:absolute 1)
                  :top (:relative 1)
                  :right (:absolute 1)
                  :bottom (:relative 0))
                (climi::normalize-margin-spec '(:left (:absolute 1)
                                                :top (:relative 1)
                                                :bottom 0)
                                              '(:left (:absolute 1)
                                                :top (:absolute 1)
                                                :right (:absolute 1)
                                                :bottom (:absolute 1)))))))

(test normalize-margin-spec.validation
  (for-all ((spec (gen-invalid-margin-spec)))
    (let ((default '(:left (:relative 0)
                     :top (:relative 0)
                     :right (:relative 0)
                     :bottom (:relative 0))))
      (signals error (climi::normalize-margin-spec spec default)))))

(test validate-parse-space
  (for-all ((spec (gen-invalid-space-spec)))
    (is (not (typep spec 'climi::space-spec)))))
