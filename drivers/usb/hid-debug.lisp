(in-package :mezzano.driver.usb.hid)

(defconstant +main-items+
  '(:unused-00
    :unused-01
    :unused-02
    :unused-03
    :unused-04
    :unused-05
    :unused-06
    :unused-07
    :input
    :output
    :collection
    :feature
    :end-collection))

(defconstant +global-items+
  '(:usage-page
    :logical-minimum
    :logical-maximum
    :physical-minimum
    :physical-maximum
    :unit-exponent
    :unit
    :report-size
    :report-id
    :report-count
    :push
    :pop))

(defconstant +local-items+
  '(:usage
    :usage-minimum
    :usage-maximum
    :designator-idx
    :designator-minimum
    :designator-maximum
    :string-idx
    :string-minimum
    :string-maximum
    :delimiter))

(defconstant +reserved-items+
  '())

(defconstant +type->tag-table+
  '(#.+main-items+
    #.+global-items+
    #.+local-items+
    #.+reserved-items+))

(defconstant +collection-types+
  '(:physical
    :application
    :logical
    :report
    :named-array
    :usage-switch
    :usage-modifier))

(defun print-report-item (stream state buf offset header size type data)
  (format stream "~2,'0X " (aref buf offset))

  (cond ((= size 0)
         (format t "            "))
        ((= size 1)
         (format t "~2,'0X          " (aref buf (+ offset 1))))
        ((= size 2)
         (format stream "~2,'0X ~2,'0X       "
                 (aref buf (+ offset 1))
                 (aref buf (+ offset 2))))
        ((= size 4)
         (format stream "~2,'0X ~2,'0X ~2,0X ~2,0X "
                 (aref buf (+ offset 1))
                 (aref buf (+ offset 2))
                 (aref buf (+ offset 3))
                 (aref buf (+ offset 4)))))
  (let ((tag (nth (ldb (byte 4 4) header)
                  (nth (ldb (byte 2 2) header)
                       +type->tag-table+))))
    (cond ((and (eq type :local) (eq tag :usage))
           (format stream "~A ~A ~A (~A)~%"
                   type
                   tag
                   data
                   (get-page-entry (parse-state-value state :page) data)))
          ((and (eq type :main) (eq tag :collection))
           (format stream "~A ~A ~A (~A)~%"
                   type
                   tag
                   data
                   (nth data +collection-types+)))
          (T
           (format stream "~A ~A ~A~%"
                   type
                   tag
                   data)))))
