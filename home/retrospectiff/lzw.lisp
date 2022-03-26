
(in-package :retrospectiff.compression)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +clear-code+ 256)
  (defconstant +end-of-information-code+ 257)
  (defconstant +first-entry+ 258)
  (defconstant +last-code+ 4093)
  (defconstant +max-code+ 4094))

(defun lzw-encode (raw-vector)
  (typecase raw-vector
    (string
     (lzw-encode
      (map 'vector #'char-code raw-vector)))
    (vector
     (let ((vector-hash (make-hash-table :test 'equalp))
           (next-entry)
           (bit-count 0)
           (output (make-array 256
                               :element-type '(unsigned-byte 8)
                               :fill-pointer 0
                               :adjustable t)))
       (labels ((initialize-vector-table ()
                  (clrhash vector-hash)
                  (setf next-entry +first-entry+))
                (lookup-vector (vector)
                  (cond ((= (length vector) 1)
                         (elt vector 0))
                        ((> (length vector) 1)
                         (gethash vector vector-hash))
                        (t (error "huh?"))))
                (add-vector (vector)
                  (setf (gethash vector vector-hash) next-entry)
                  (incf next-entry))
                (code-size ()
                  (integer-length next-entry))
                (write-code (int)
                  (let ((new-fill-pointer
                         (1+ (ash (+ bit-count (code-size)) -3))))
                    (ensure-array-size-and-set-fill-pointer
                     output new-fill-pointer)
                    (set-bits output bit-count (+ bit-count (code-size))
                              int))
                  (incf bit-count (code-size))))
         (initialize-vector-table)
         (write-code +clear-code+)
         (let ((omega))
           (loop for k across raw-vector
              do
              (let ((cat (concatenate 'vector omega (vector k))))
                (if (lookup-vector cat)
                    (setf omega cat)
                    (progn
                      (write-code (lookup-vector omega))
                      (add-vector cat)
                      (setf omega (vector k))
                      (when (= next-entry +max-code+)
                        (write-code +clear-code+)
                        (initialize-vector-table))))))
           (write-code (lookup-vector omega)))
         (write-code +end-of-information-code+)
         output)))))

(defconstant +initial-code-size+ 9)

;; while ((Code = GetNextCode()) != EoiCode) {
;;     if (Code == ClearCode) { 
;;         InitializeTable(); 
;;         Code = GetNextCode(); 
;;         if (Code == EoiCode) 
;;             break; 
;;         WriteString(StringFromCode(Code)); 
;;         OldCode = Code; 
;;     }  /* end of ClearCode case */ 
;;     else { 
;;         if (IsInTable(Code)) { 
;;             WriteString(StringFromCode(Code)); 
;;             AddStringToTable(StringFromCode(OldCode) +
;;                              FirstChar(StringFromCode(Code))); 
;;             OldCode = Code; 
;;         } else { 
;;             OutString = StringFromCode(OldCode) + 
;;                 FirstChar(StringFromCode(OldCode)); 
;;             WriteString(OutString); 
;;             AddStringToTable(OutString); 
;;             OldCode = Code; 
;;         } 
;;     } /* end of not-ClearCode case */ 
;;  } /* end of while loop */ 

(defun lzw-decode-codes (compressed-vector)
  (let ((bit-offset 0)
        (code-size +initial-code-size+)
        (next-entry +first-entry+))
    (labels ((get-next-code ()
               (let ((code (get-bits compressed-vector
                                     bit-offset
                                     (incf bit-offset code-size))))
                 (cond ((= next-entry (- (expt 2 code-size) 2))
                        (incf code-size)
                        code)
                       ((= code +clear-code+)
                        (setf code-size +initial-code-size+)
                        (get-next-code))
                       (t code)))))
      (loop for code = (get-next-code)
         with old-code
         while (not (= code +end-of-information-code+))
         when old-code do (incf next-entry)
         do (setf old-code code)
         collect code))))

(defun lzw-decode (compressed-vector &key stream)
  (let ((input-bit-offset 0)
        (output-byte-offset 0)
        (code-size +initial-code-size+)
        (next-entry +first-entry+)
        vector-hash
        (output (make-array 256
                            :element-type '(unsigned-byte 8)
                            :fill-pointer 0
                            :adjustable t)))
    (flet ((initialize-vector-table ()
             (setf vector-hash (make-hash-table :test 'equal)
                   next-entry +first-entry+
                   code-size +initial-code-size+))
           (get-next-code ()
             (let ((code (get-bits compressed-vector
                                   input-bit-offset
                                   (incf input-bit-offset code-size))))
               
               code))
           (vector-from-code (code)
             (if (< code +clear-code+)
                 (vector code)
                 (gethash code vector-hash)))
           (add-vector-to-table (vector)
             (setf (gethash next-entry vector-hash) vector)
             (incf next-entry)
             (when (= next-entry (- (expt 2 code-size) 1))
               (incf code-size)))
           (write-code (int)
             (ensure-array-size-and-set-fill-pointer
              output output-byte-offset)
             (setf (aref output output-byte-offset) int)
             (setf (fill-pointer output)
                   (incf output-byte-offset))))
      (flet ((write-decoded-vector (vector)
               (loop for code across vector
                  do 
                    (if stream
                        (write-byte code stream)
                        (write-code code)))))
        (loop for code = (get-next-code)
           with old-code
           while (not (= code +end-of-information-code+))
           do 
             (cond ((= code +clear-code+)
                    (initialize-vector-table)
                    (setf old-code nil))
                   (t
                    (let ((string (vector-from-code code)))
                      (if string
                          (progn
                            (write-decoded-vector string)
                            (when old-code
                              (add-vector-to-table
                               (concatenate '(vector (unsigned-byte 8) *)
                                            (vector-from-code old-code)
                                            (subseq string 0 1)))))
                          (let ((v (concatenate '(vector (unsigned-byte 8) *)
                                                (vector-from-code old-code)
                                                (subseq (vector-from-code old-code)
                                                        0 1))))
                            (write-decoded-vector v)
                            (add-vector-to-table v))))
                    (setf old-code code))))))
    (unless stream output)))

