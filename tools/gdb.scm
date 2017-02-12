(use-modules ((gdb) #:renamer (symbol-prefix-proc 'gdb:)))
(use-modules (ice-9 format))
(use-modules (ice-9 rdelim))

(define all-symbols '())

(define (file-write-date object)
  (if (or (not (string? object))
          (access? object F_OK))
      (stat:mtime (stat object))
      #f))

(define (parse-symbol-file-line line)
  (let* ((index (string-index line #\Space))
         (address (substring line 0 index))
         (name (substring line (1+ index))))
    (list (string->number address 16)
          name)))

(define (read-symbol-file path)
  (let ((result '()))
    (with-input-from-file path
      (lambda ()
        (do ((line (read-line) (read-line)))
            ((eof-object? line))
          (set! result (cons (parse-symbol-file-line line) result)))))
    (sort (reverse result)
          (lambda (x y) (< (car x) (car y))))))

(define (load-symbols path)
  (format #t "Loading symbols from ~a~%" path)
  (set! all-symbols (read-symbol-file path)))

(define (ldsym)
  (let ((f1 (file-write-date "lispos.map"))
        (f2 (file-write-date "new.map")))
    (cond ((and f1 f2)
           (load-symbols (if (> f1 f2)
                             "lispos.map"
                             "new.map")))
          (f1
           (load-symbols "lispos.map"))
          (f2
           (load-symbols "new.map")))))

(define (lookup name)
  (define (frob list)
    (cond ((eq? list '())
           #f)
          ((string-ci=? name (car (cdr (car list))))
           (car (car list)))
          (#t
           (frob (cdr list)))))
  (frob all-symbols))

(define (rlookup address)
  (define (frob list last)
    (cond ((or (eq? list '())
               (< address (car (car list))))
           last)
          (#t
           (frob (cdr list) (car list)))))
  (define sym (frob all-symbols (list 0 "ukn")))
  (define base (car sym))
  (define name (car (cdr sym)))
  (list name (- address base)))

(define (break name)
  (define addr (lookup name))
  (when (not addr)
    (error "Unknown symbol" name))
  (gdb:execute (format #f "break *0x~x" addr))
  addr)

(define (trace)
  (define pc (gdb:value->integer (gdb:parse-and-eval "$pc")))
  (define loc (rlookup pc))
  (define asm (gdb:arch-disassemble (gdb:current-arch) pc #:count 1))
  (format #t "~8,'0x ~A+~D: ~a~%" pc (car loc) (car (cdr loc)) (cdr (assoc 'asm (car asm)))))

(gdb:register-command!
 (gdb:make-command "ltrace"
                   #:invoke (lambda (self args from-tty)
                              (trace))))

;; This doesn't seem to work?
;; make-command/register-command doesn't work either.
;;(gdb:execute "define hook-stop
;;ltrace
;;end")

(define (read-value-type address type)
  (let* ((ulong (gdb:arch-ulong-type (gdb:current-arch)))
         (vaddr (gdb:make-value address #:type ulong))
         (ptr (gdb:value-cast vaddr (gdb:type-pointer type))))
    (call-with-current-continuation
     (lambda (exit)
       (catch 'gdb:memory-error
              (lambda ()
                (gdb:value->integer (gdb:value-dereference ptr)))
              (lambda (key . args)
                (exit #f)))))))

(define (read-value address)
  (read-value-type address (gdb:arch-ulong-type (gdb:current-arch))))

(define (1d-aref value index)
  (case (object-tag value)
    ((#f) => #f)
    ((#b000000) (oref-t value index))
    ((#b000101) ; ub8
     (read-value-type (+ (- value 9) 8 index)
                      (gdb:arch-uchar-type (gdb:current-arch))))))

(define (read-string string-value)
  (let ((length (fixnum->integer (oref-t string-value 3)))
        (storage (oref-t string-value 0))
        (chars '()))
    (do ((i 0 (1+ i)))
        ((>= i length))
      (let ((ch (1d-aref storage i)))
        (set! chars (cons (integer->char ch) chars))))
    (reverse-list->string chars)))

(define (nil-value)
  #x200009)

(define (value-fixnum? value)
  (and value
       (not (logbit? 0 value))))

(define (fixnum->integer value)
  (logior (ash value -1)
          (if (logbit? 63 value)
              (ash -1 63)
              0)))

(define (value-tag value)
  (logand value #b1111))

(define (value-cons? value)
  (and value
       (eq? (value-tag value) 3)))

(define (value-car value)
  (read-value (- value 3)))

(define (value-cdr value)
  (read-value (+ (- value 3) 8)))

(define (oref-t value index)
  (read-value (+ (- value 9)
                 (* (1+ index) 8))))

(define (object-tag value)
  (logand (ash (oref-t value -1) -2)
          #b111111))

(define (object-header-data value)
  (ash (oref-t value -1) -8))

(define (value-symbol-name value)
  (oref-t value 0))

(define (display-value-1 value depth-limit)
  (cond ((not value)
         (format #t "#<unreadable-value>"))
        ((< depth-limit 0)
         (format #t "#<truncated-value {~x}>" value))
        ((value-fixnum? value)
         (format #t "#<fixnum ~a>" (fixnum->integer value)))
        ((eq? (value-tag value) #b0001)
         (format #t "#<dx-root {~x}>" value))
        ((eq? (value-tag value) #b0011)
         (display "(")
         (display-value-1 (value-car value) (1- depth-limit))
         (do ((i (value-cdr value)
                 (value-cdr i)))
             ((or (< depth-limit 0)
                  (not (value-cons? i)))
              (cond ((< depth-limit 0)
                     (display " . #<truncated>"))
                    ((not (eq? i (nil-value)))
                     (display " . ")
                     (display-value-1 i (1- depth-limit))))
              (display ")"))
           (display " ")
           (display-value-1 (value-car i) (1- depth-limit))
           (set! depth-limit (1- depth-limit))))
        ((eq? (value-tag value) #b0101)
         (format #t "#<tag-5 {~x}>" value))
        ((eq? (value-tag value) #b0111)
         (format #t "#<byte-specifier {~x}>" value))
        ((eq? (value-tag value) #b1001)
         (case (object-tag value)
           ((#f) (format #t "#<unreadable-object {~x}>" value))
           ((#b110000)
            ;; Symbol.
            (display-value-1 (value-symbol-name value)
                             (1- depth-limit)))
           ((#b011100 #b011101)
            ;; Strings.
            (display (read-string value)))
           (else
            (format #t "#<object-~b {~x}>" (object-tag value) value))))
        ((eq? (value-tag value) #b1011)
         (format #t "#<character {~x}>" value))
        ((eq? (value-tag value) #b1101)
         (format #t "#<single-float {~x}>" value))
        ((eq? (value-tag value) #b1111)
         (format #t "#<gc-forward {~x}>" value))))

(define (display-value value)
  (display-value-1 value 10))

(define (print-line . args)
  (if (pair? args)
      (begin
        (display-value (car args))
        (apply print-line (cdr args)))
      (newline)))

(define (function-pool-object value index)
  (let* ((address (logand value -16))
         (mc-size (* (logand (ash (object-header-data value) -8)
                          #xFFFF)
                     16)))
    (read-value (+ address mc-size (* index 8)))))

(define (function-name value)
  (function-pool-object value 0))

(define (pc-to-function pc)
  ;; Walk backwards looking for an object header with a function type and
  ;; an appropriate entry point.
  (let ((address (logand pc -16)))
    (while #t
      (let ((potential-header-type (logand (ash (read-value address) -2) #b111111)))
        (when (and
               (eq? potential-header-type #b111100)
               ;; Currently the entry point of every non-closure function
               ;; points to the base-address + 16.
               (eq? (+ address 16) (read-value (+ address 8))))
          (break (+ address 9)))
        (set! address (- address 16))))))

(define (where)
  (print-line (function-name (pc-to-function (gdb:value->integer (gdb:frame-read-register (gdb:newest-frame) "pc"))))))

(define (whereis addr)
  (print-line (function-name (pc-to-function addr))))

(define (unwind)
  (define (frob addr)
    (format #t "~8,'0X ~A~%" addr (rlookup addr)))
  (frob (gdb:value->integer (gdb:frame-read-register (gdb:newest-frame) "pc")))
  (frob (gdb:value->integer (gdb:frame-read-register (gdb:newest-frame) "x30")))
  (do ((fp (gdb:value->integer (gdb:frame-read-register (gdb:newest-frame) "x29"))
           (read-value fp)))
      ((or (not fp)
           (= fp 0)))
    (format #t "~8',0X " fp)
    (frob (read-value (+ fp 8)))))

(define (unwind2)
  (define (frob addr)
    (format #t "~8,'0X " addr)
    (print-line (function-name (pc-to-function addr))))
  (frob (gdb:value->integer (gdb:frame-read-register (gdb:newest-frame) "pc")))
  (frob (gdb:value->integer (gdb:frame-read-register (gdb:newest-frame) "x30")))
  (do ((fp (gdb:value->integer (gdb:frame-read-register (gdb:newest-frame) "x29"))
           (read-value fp)))
      ((or (not fp)
           (= fp 0)))
    (format #t "~8',0X " fp)
    (frob (read-value (+ fp 8)))))
