(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :mcclim))

(cl:in-package #:clim-user)

; LTAG-start:scheduler-part1
(defvar *days* #("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))

;; Alist of day number and appointment strings
(defvar *test-data*
  '((0) (1 "Dentist") (2 "Staff meeting") (3 "Performance Evaluation" "Bowling")
    (4 "Interview at ACME" "The Simpsons") (5 "TGIF") (6 "Sailing")))

(define-presentation-type weekday ())

(define-presentation-method accept
    ((type weekday) stream (view textual-view) &key)
  (values (completing-from-suggestions (stream)
            (dotimes (i 7)
              (suggest (aref *days* i) i)))))

(define-presentation-method present
    (daynumber (type weekday) stream (view textual-view) &key)
  (write-string (aref *days* daynumber) stream))


(define-application-frame scheduler ()
  ((appointments :initarg :appointments :initform *test-data*)
   (current-day :initform nil))
  (:panes (scheduler-display :application
                             :display-function '(display-appointments))
          (interactor :interactor))
  (:layouts (default-layout
             (vertically ()
               scheduler-display
               interactor))
            (alternative-layout
             (horizontally ()
               interactor
               scheduler-display)))
  (:menu-bar t))

;;; Chooses which day to see in detail,
(define-scheduler-command (com-select-day :name t :menu t)
    ((day 'weekday :gesture :select))
  (with-slots (current-day) *application-frame*
    (setq current-day day)))

;;; Show weekly summary.
(define-scheduler-command (com-show-summary :name t :menu t) ()
  (with-slots (current-day) *application-frame*
    (setq current-day nil)))

(define-scheduler-command (com-toggle-layout :name t :menu t) ()
  (with-accessors ((layout frame-current-layout)) *application-frame*
    (setf layout (if (eq layout 'default-layout)
                     'alternative-layout
                     'default-layout))))
; LTAG-end
; LTAG-start:scheduler-part2
;;; Complex display function, shows two completely different displays.
(defmethod display-appointments ((frame scheduler) pane)
  (clear-output-record (stream-output-history pane))
  (with-slots (current-day appointments) frame
    (if (null current-day)
        (show-weekly-summary pane appointments)
        (show-appointments pane
                           current-day
                           (rest (assoc current-day appointments))))))

;;; Show a summary of the week, with an appointment count for each
;;; day. You can see the appointments for a specific day by clicking on
;;; the day name.
(defun show-weekly-summary (pane appointments)
  (formatting-table (pane)              ; Table headings
    (formatting-row (pane)
      (with-drawing-options (pane :text-face :bold)
        (formatting-cell (pane)
          (write-string "Day of week" pane))
        (formatting-cell (pane)
          (write-string "number of appointments" pane))))
    (dolist (day appointments)
      (formatting-row (pane)
        (formatting-cell (pane)
          (present (first day) 'weekday :stream pane))
        (formatting-cell (pane)
          (format pane "~D appointment~:P"
                  (length (rest day))))))))

;;; Show detailed appointment list for day
(defun show-appointments (pane current-day current-day-appointments)
  ;; Show all days at top so you can switch to another
  ;; day with one click.
  (dotimes (day 7)
    (with-text-face (pane (if (eql day current-day) ':bold ':roman))
       (present day 'weekday :stream pane))
    (write-string " " pane))
  (terpri pane) (terpri pane)
  ;; Show all the appointments, one per line
  (write-string "Appointments for " pane)
  (present current-day 'weekday :stream pane)
  (terpri pane) (terpri pane)
  (dolist (appointment current-day-appointments)
    (write-string appointment pane)
    (terpri pane)))
; LTAG-end
