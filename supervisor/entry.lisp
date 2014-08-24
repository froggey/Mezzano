(in-package :mezzanine.supervisor)

;;; FIXME: Should not be here.
;;; >>>>>>

(defun string-length (string)
  (assert (sys.int::character-array-p string))
  (sys.int::%array-like-ref-t string 3))

(defvar *allocator-lock*)
(defvar *boot-allocation-mode*)

(defvar sys.int::*2g-allocation-bump*)
(defvar sys.int::*allocation-bump*)

(defvar sys.int::*boot-area-base*)
(defvar sys.int::*boot-area-bump*)

(defmacro with-gc-deferred (&body body)
  `(call-with-gc-deferred (flet ((with-gc-deferred-thunk () ,@body))
                            (declare (dynamic-extent #'with-gc-deferred-thunk))
                            #'with-gc-deferred-thunk)))

;; TODO?
(defun call-with-gc-deferred (thunk)
  (funcall thunk))

(defun find-extent-named (name largep)
  (cond ((store-extent-p name) name)
        (t (dolist (extent *extent-table*
                    (error "can't find extent..."))
             (when (and (or (eql (store-extent-type extent) name)
                            (and (eql name :wired)
                                 (eql (store-extent-type extent) :pinned)
                                 (store-extent-wired-p extent)))
                        (not (store-extent-finished-p extent))
                        (eql (store-extent-large-p extent) largep))
               (return extent))))))

(defun %allocate-object (tag data size area)
  (let ((words (1+ size)))
    (when (oddp words)
      (incf words))
    (cond (*boot-allocation-mode*
           (assert (eql area :wired))
           (assert (<= (+ sys.int::*boot-area-bump* (* words 8)) #x200000))
           (with-symbol-spinlock (*allocator-lock*)
             (let ((addr (+ sys.int::*boot-area-base* sys.int::*boot-area-bump*)))
               (incf sys.int::*boot-area-bump* (* words 8))
               ;; Write array header.
               (setf (sys.int::memref-unsigned-byte-64 addr 0)
                     (logior (ash tag sys.int::+array-type-shift+)
                             (ash data sys.int::+array-length-shift+)))
               (sys.int::%%assemble-value addr sys.int::+tag-object+))))
          (t (with-symbol-spinlock (*allocator-lock*)
               (let* ((area (find-extent-named (or area :dynamic) (>= (* words 8) (* 256 1024))))
                      (addr (+ (store-extent-virtual-base area) (store-extent-bump area))))
                 (assert (<= (+ (store-extent-bump area) (* words 8)) (store-extent-size area)))
                 (incf (store-extent-bump area) (* words 8))
                 ;; Write array header.
                 (setf (sys.int::memref-unsigned-byte-64 addr 0)
                       (logior (ash tag sys.int::+array-type-shift+)
                               (ash data sys.int::+array-length-shift+)))
                 (sys.int::%%assemble-value addr sys.int::+tag-object+)))))))

(defun sys.int::make-simple-vector (size &optional area)
  (%allocate-object sys.int::+object-tag-array-t+ size size area))

(defun sys.int::%make-struct (size &optional area)
  (%allocate-object sys.int::+object-tag-structure-object+ size size area))

(defun sys.int::cons-in-area (car cdr &optional area)
  (cond (*boot-allocation-mode*
         (assert (eql area :wired))
         (assert (<= (+ sys.int::*boot-area-bump* (* 4 8)) #x200000))
         (with-symbol-spinlock (*allocator-lock*)
           (let ((addr (+ sys.int::*boot-area-base* sys.int::*boot-area-bump*)))
             (incf sys.int::*boot-area-bump* (* 4 8))
             ;; Set header.
             (setf (sys.int::memref-unsigned-byte-64 addr 0) (ash sys.int::+object-tag-cons+ sys.int::+array-type-shift+))
             ;; Set car/cdr.
             (setf (sys.int::memref-t addr 2) car
                   (sys.int::memref-t addr 3) cdr)
             (sys.int::%%assemble-value (+ addr 16) sys.int::+tag-cons+))))
        (t (with-symbol-spinlock (*allocator-lock*)
               (let* ((area (find-extent-named (or area :dynamic-cons) nil))
                      (addr (+ (store-extent-virtual-base area) (store-extent-bump area))))
                 (case (store-extent-type area)
                   (:dynamic-cons
                    (assert (<= (+ (store-extent-bump area) 16) (store-extent-size area)))
                    (incf (store-extent-bump area) 16)
                    (setf (sys.int::memref-t addr 0) car
                          (sys.int::memref-t addr 1) cdr)
                    (sys.int::%%assemble-value addr sys.int::+tag-cons+))
                   (t (assert (<= (+ (store-extent-bump area) 32) (store-extent-size area)))
                      (incf (store-extent-bump area) 32)
                      ;; Write array header.
                      (setf (sys.int::memref-unsigned-byte-64 addr 0) (ash sys.int::+object-tag-cons+ sys.int::+array-type-shift+))
                      ;; Set car/cdr.
                      (setf (sys.int::memref-t addr 2) car
                            (sys.int::memref-t addr 3) cdr)
                      (sys.int::%%assemble-value (+ addr 16) sys.int::+tag-cons+))))))))

(defun cons (car cdr)
  (sys.int::cons-in-area car cdr))

(defun sys.int::make-closure (function environment)
  "Allocate a closure object."
  ;; FIXME: Obey the +/-2GB jmp limit. (switch to indirect when exceeded.)
  (check-type function function)
  (with-gc-deferred
    (let* ((fn (%allocate-object sys.int::+object-tag-closure+ #x2000200 5 :pinned))
           (address (logand (sys.int::lisp-object-address fn) (lognot 15)))
           (entry-point (sys.int::%array-like-ref-unsigned-byte-64 function 0))
           ;; Jmp's address is entry-point - <address-of-instruction-after-jmp>
           (rel-entry-point (- entry-point (+ address (* 7 4)))))
      (setf
       ;; Entry point
       (sys.int::memref-unsigned-byte-64 address 1) (+ address 16)
       ;; The code.
       ;; mov64 :rbx (:rip 17)/pool[1]
       (sys.int::memref-unsigned-byte-32 address 4) #x111D8B48
       ;; jmp entry-point
       (sys.int::memref-unsigned-byte-32 address 5) #xE9000000
       ;; jmp's rel32 address ended up being nicely aligned. lucky!
       (sys.int::memref-signed-byte-32 address 6) rel-entry-point
       (sys.int::memref-unsigned-byte-32 address 7) #xCCCCCCCC
       ;; Initialize constant pool
       (sys.int::memref-t address 4) function
       (sys.int::memref-t address 5) environment)
      fn)))

(defun make-symbol (name)
  (check-type name string)
  ;; FIXME: Copy name into the wired area and unicode normalize it.
  (with-gc-deferred ()
    (let* ((symbol (%allocate-object sys.int::+object-tag-symbol+ 0 5 :wired)))
      ;; symbol-name.
      (setf (sys.int::%array-like-ref-t symbol 0) name)
      (makunbound symbol)
      (setf (sys.int::symbol-fref symbol) nil
            (symbol-plist symbol) nil
            (symbol-package symbol) nil)
      symbol)))

(defun sys.int::%allocate-array-like (tag word-count length &optional area)
  (%allocate-object tag length word-count area))

(defun stack-base (stack)
  (store-extent-virtual-base stack))

(defun stack-size (stack)
  (store-extent-size stack))

(defun %allocate-stack (size)
  ;; 2M align stacks.
  (incf size #x1FFFFF)
  (setf size (logand size (lognot #x1FFFFF)))
  (let* ((addr (with-symbol-spinlock (*allocator-lock*)
                 (prog1 sys.int::*allocation-bump*
                   (incf sys.int::*allocation-bump* (+ size #x200000)))))
         (extent (make-store-extent :store-base nil
                                    :virtual-base addr
                                    :size size
                                    :bump size
                                    :wired-p nil
                                    :type :stack
                                    :zero-fill t)))
    (setf *extent-table* (sys.int::cons-in-area extent *extent-table* :wired))
    extent))

;; TODO.
(defun sleep (seconds)
  nil)

(defun sys.int::raise-undefined-function (fref)
  (debug-write-string "Undefined function ")
  (let ((name (sys.int::%array-like-ref-t fref sys.int::+fref-name+)))
    (cond ((consp name)
           (debug-write-string "(")
           (debug-write-string (symbol-name (car name)))
           (debug-write-string " ")
           (debug-write-string (symbol-name (car (cdr name))))
           (debug-write-line ")"))
          (t (debug-write-line (symbol-name name)))))
  (sys.int::%sti)
  (loop))

(defun sys.int::raise-unbound-error (symbol)
  (debug-write-string "Unbound symbol ")
  (debug-write-line (symbol-name symbol))
  (sys.int::%sti)
  (loop))

(in-package :sys.int)

(defstruct (cold-stream (:area :wired)))

(in-package :mezzanine.supervisor)

(defvar *cold-unread-char*)

(defun sys.int::cold-write-char (c stream)
  (declare (ignore stream))
  (debug-write-char c))

(defun sys.int::cold-start-line-p (stream)
  (declare (ignore stream))
  (debug-start-line-p))

(defun sys.int::cold-read-char (stream)
  (declare (ignore stream))
  (cond (*cold-unread-char*
         (prog1 *cold-unread-char*
           (setf *cold-unread-char* nil)))
        (t (debug-read-char))))

(defun sys.int::cold-unread-char (character stream)
  (declare (ignore stream))
  (when *cold-unread-char*
    (error "Multiple unread-char!"))
  (setf *cold-unread-char* character))

;;; <<<<<<

(defvar *boot-information-page*)

(defstruct (disk
             (:area :wired))
  device
  n-sectors
  sector-size
  max-transfer
  read-fn
  write-fn)

(defvar *disks*)

(defstruct (store-extent
             (:area :wired))
  store-base
  virtual-base
  size
  bump
  wired-p
  large-p
  finished-p
  type
  zero-fill)

(defvar *paging-disk*)
(defvar *extent-table*)

(defconstant +n-physical-buddy-bins+ 32)
(defconstant +buddy-bin-size+ 16)

(defconstant +boot-information-boot-uuid-offset+ 0)
(defconstant +boot-information-physical-buddy-bins-offset+ 16)

(defun boot-uuid (offset)
  (check-type offset (integer 0 15))
  (sys.int::memref-unsigned-byte-8 *boot-information-page* offset))

(defun register-disk (device n-sectors sector-size max-transfer read-fn write-fn)
  (when (> sector-size +4k-page-size+)
    (debug-write-line "Ignoring device with sector size larger than 4k."))
  (let* ((disk (make-disk :device device
                          :sector-size sector-size
                          :n-sectors n-sectors
                          :max-transfer max-transfer
                          :read-fn read-fn
                          :write-fn write-fn))
         (page (or (allocate-physical-pages (ceiling (max +4k-page-size+ sector-size) +4k-page-size+))
                   ;; I guess this could happen on strange devices with sector sizes > 4k.
                   (error "Unable to allocate memory when examining device ~S!" device)))
         (page-addr (+ +physical-map-base+ (* page +4k-page-size+))))
    (setf *disks* (sys.int::cons-in-area disk *disks* :wired))
    ;; Read first 4k, figure out what to do with it.
    (or (funcall read-fn device 0 (ceiling +4k-page-size+ sector-size) page-addr)
        (progn
          (release-physical-pages page (ceiling (max +4k-page-size+ sector-size) +4k-page-size+))
          (error "Unable to read first sector on device ~S!" device)))
    ;; Search for a Mezzanine header here.
    ;; TODO: Scan for partition maps.
    (when (and
           (not *paging-disk*)
           ;; Match magic.
           (loop
              for byte in '(#x00 #x4D #x65 #x7A #x7A #x61 #x6E #x69 #x6E #x65 #x49 #x6D #x61 #x67 #x65 #x00)
              for offset from 0
              do (when (not (eql (sys.int::memref-unsigned-byte-8 page-addr offset) byte))
                   (return nil))
              finally (return t))
           ;; Match boot UUID.
           (loop
              for offset from 0 below 16
              do (when (not (eql (sys.int::memref-unsigned-byte-8 page-addr (+ 16 offset))
                                 (boot-uuid offset)))
                   (return nil))
              finally (return t)))
      (debug-write-line "Found boot image!")
      (setf *paging-disk* disk)
      ;; Initialize the extent table.
      (setf *extent-table* '())
      (dotimes (i (sys.int::memref-unsigned-byte-32 (+ page-addr 36) 0))
        (let* ((addr (+ page-addr 96 (* i 40)))
               (flags (sys.int::memref-unsigned-byte-64 addr 4))
               (extent (make-store-extent :store-base (sys.int::memref-unsigned-byte-64 addr 0)
                                          :virtual-base (sys.int::memref-unsigned-byte-64 addr 1)
                                          :size (sys.int::memref-unsigned-byte-64 addr 2)
                                          :bump (sys.int::memref-unsigned-byte-64 addr 3)
                                          :wired-p (logbitp 3 flags)
                                          :large-p (logbitp 4 flags)
                                          :finished-p (logbitp 5 flags)
                                          :type (ecase (ldb (byte 3 0) flags)
                                                  (0 :pinned)
                                                  (1 :pinned-2g)
                                                  (2 :dynamic)
                                                  (3 :dynamic-cons)
                                                  (4 :nursery)
                                                  (5 :stack)))))
          (setf *extent-table*
                (sys.int::cons-in-area
                 extent
                 *extent-table*
                 :wired))
          ;; Consider switching over to proper allocation.
          (when (eql sys.int::*boot-area-base* (store-extent-virtual-base extent))
            (setf (store-extent-bump extent) sys.int::*boot-area-bump*
                  *boot-allocation-mode* nil)))))
    ;; Release the pages.
    (release-physical-pages page (ceiling (max +4k-page-size+ sector-size) +4k-page-size+))))

(defvar *vm-lock*)

(defconstant +page-table-present+        #x001)
(defconstant +page-table-write+          #x002)
(defconstant +page-table-user+           #x004)
(defconstant +page-table-write-through+  #x008)
(defconstant +page-table-cache-disabled+ #x010)
(defconstant +page-table-accessed+       #x020)
(defconstant +page-table-dirty+          #x040)
(defconstant +page-table-page-size+      #x080)
(defconstant +page-table-global+         #x100)
(defconstant +page-table-address-mask+   #x000FFFFFFFFFF000)

(defun wait-for-page-via-interrupt (interrupt-frame extent address)
  (declare (ignore interrupt-frame))
  (with-mutex (*vm-lock*)
    ;; Examine the page table, if there's a present entry then the page
    ;; was mapped while acquiring the VM lock. Just return.
    (let ((cr3 (+ +physical-map-base+ (logand (sys.int::%cr3) (lognot #xFFF))))
          (pml4e (ldb (byte 9 39) address))
          (pdpe (ldb (byte 9 30) address))
          (pde (ldb (byte 9 21) address))
          (pte (ldb (byte 9 12) address)))
      (when (not (logtest +page-table-present+ (sys.int::memref-unsigned-byte-64 cr3 pml4e)))
        ;; No PDP. Allocate one.
        (let* ((frame (or (allocate-physical-pages 1)
                          (progn (debug-write-line "Aiee. No memory.")
                                 (loop))))
               (addr (+ +physical-map-base+ (ash frame 12))))
          (dotimes (i 512)
            (setf (sys.int::memref-unsigned-byte-64 addr i) 0))
          (setf (sys.int::memref-unsigned-byte-64 cr3 pml4e) (logior (ash frame 12)
                                                                     +page-table-present+
                                                                     +page-table-write+))))
      (let ((pdp (+ +physical-map-base+ (logand (sys.int::memref-unsigned-byte-64 cr3 pml4e) +page-table-address-mask+))))
        (when (not (logtest +page-table-present+ (sys.int::memref-unsigned-byte-64 pdp pdpe)))
          ;; No PDir. Allocate one.
          (let* ((frame (or (allocate-physical-pages 1)
                            (progn (debug-write-line "Aiee. No memory.")
                                   (loop))))
                 (addr (+ +physical-map-base+ (ash frame 12))))
            (dotimes (i 512)
              (setf (sys.int::memref-unsigned-byte-64 addr i) 0))
            (setf (sys.int::memref-unsigned-byte-64 pdp pdpe) (logior (ash frame 12)
                                                                      +page-table-present+
                                                                      +page-table-write+))))
        (let ((pdir (+ +physical-map-base+ (logand (sys.int::memref-unsigned-byte-64 pdp pdpe) +page-table-address-mask+))))
          (when (not (logtest +page-table-present+ (sys.int::memref-unsigned-byte-64 pdir pde)))
            ;; No PT. Allocate one.
            (let* ((frame (or (allocate-physical-pages 1)
                              (progn (debug-write-line "Aiee. No memory.")
                                     (loop))))
                   (addr (+ +physical-map-base+ (ash frame 12))))
              (dotimes (i 512)
                (setf (sys.int::memref-unsigned-byte-64 addr i) 0))
              (setf (sys.int::memref-unsigned-byte-64 pdir pde) (logior (ash frame 12)
                                                                        +page-table-present+
                                                                        +page-table-write+))))
          (let ((pt (+ +physical-map-base+ (logand (sys.int::memref-unsigned-byte-64 pdir pde) +page-table-address-mask+))))
            (when (not (logtest +page-table-present+ (sys.int::memref-unsigned-byte-64 pt pte)))
              ;; No page allocated. Allocate a page and read the data.
              (let* ((frame (or (allocate-physical-pages 1)
                                (progn (debug-write-line "Aiee. No memory.")
                                       (loop))))
                     (addr (+ +physical-map-base+ (ash frame 12))))
                (cond ((store-extent-zero-fill extent)
                       (dotimes (i 512)
                         (setf (sys.int::memref-unsigned-byte-64 addr i) 0)))
                      (t (or (funcall (disk-read-fn *paging-disk*)
                                      (disk-device *paging-disk*)
                                      (* (truncate (+ (store-extent-store-base extent)
                                                      (- address (store-extent-virtual-base extent)))
                                                   +4k-page-size+)
                                         (ceiling +4k-page-size+ (disk-sector-size *paging-disk*)))
                                      (ceiling +4k-page-size+ (disk-sector-size *paging-disk*))
                                      addr)
                             (progn (debug-write-line "Unable to read page from disk")
                                    (loop)))))
                (setf (sys.int::memref-unsigned-byte-64 pt pte) (logior (ash frame 12)
                                                                        +page-table-present+
                                                                        +page-table-write+))))))))))

(defun sys.int::bootloader-entry-point (boot-information-page)
  (initialize-initial-thread)
  (setf *boot-information-page* boot-information-page
        *boot-allocation-mode* t
        *cold-unread-char* nil)
  ;; FIXME: Should be done by cold generator
  (when (not (boundp '*allocator-lock*))
    (setf *allocator-lock* :unlocked
          mezzanine.runtime::*tls-lock* :unlocked
          mezzanine.runtime::*active-catch-handlers* 'nil)
    ;; Bootstrap the defstruct system.
    ;; 1) Initialize *structure-type-type* so make-struct-definition works.
    (setf sys.int::*structure-type-type* nil)
    ;; 2) Create the real definition, with broken type.
    (setf sys.int::*structure-type-type* (sys.int::make-struct-definition
                                          'sys.int::structure-definition
                                          ;; (name accessor initial-value type read-only atomic).
                                          '((sys.int::name sys.int::structure-name nil t t nil)
                                            (sys.int::slots sys.int::structure-slots nil t t nil)
                                            (sys.int::parent sys.int::structure-parent nil t t nil)
                                            (sys.int::area sys.int::structure-area nil t t nil)
                                            (sys.int::class sys.int::structure-class nil t nil nil))
                                          nil
                                          :wired))
    ;; 3) Patch up the broken structure type.
    (setf (sys.int::%struct-slot sys.int::*structure-type-type* 0) sys.int::*structure-type-type*))
  (initialize-interrupts)
  (initialize-i8259)
  (initialize-physical-allocator)
  (initialize-threads)
  (when (not (boundp '*vm-lock*))
    (setf *vm-lock* (make-mutex "Global VM Lock")))
  (sys.int::%sti)
  (initialize-debug-serial #x3F8 4 38400)
  ;;(debug-set-output-pesudostream (lambda (op &optional arg) (declare (ignore op arg))))
  (debug-write-line "Hello, Debug World!")
  (setf *disks* '()
        *paging-disk* nil)
  (initialize-ata)
  (when (or (not *paging-disk*)
            *boot-allocation-mode*)
    (debug-write-line "Could not find boot device. Sorry.")
    (loop))
  ;; Load the extent table.
  (make-thread #'sys.int::initialize-lisp :name "Main thread")
  (finish-initial-thread))
