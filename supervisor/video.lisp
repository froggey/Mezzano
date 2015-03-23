;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

(defstruct (framebuffer
             (:area :wired))
  base-address
  width
  height
  pitch
  layout)

(defvar *current-framebuffer*)
(defvar *debug-video-x* 0)
(defvar *debug-video-y* 0)
(defvar *debug-video-col* 0)

(defun initialize-early-video ()
  ;; Trash old framebuffer.
  (setf *current-framebuffer* nil)
  (setf *debug-video-x* 0
        *debug-video-y* 0
        *debug-video-col* 0))

(defun initialize-video ()
  (let ((phys (sys.int::memref-t (+ *boot-information-page* +boot-information-framebuffer-physical-address+) 0))
        (width (sys.int::memref-t (+ *boot-information-page* +boot-information-framebuffer-width+) 0))
        (height (sys.int::memref-t (+ *boot-information-page* +boot-information-framebuffer-height+) 0))
        (pitch (sys.int::memref-t (+ *boot-information-page* +boot-information-framebuffer-pitch+) 0))
        (layout (ecase (sys.int::memref-t (+ *boot-information-page* +boot-information-framebuffer-layout+) 0)
                  (1 :x8r8g8b8))))
    (debug-print-line "Framebuffer at " phys "  " width "x" height "  layout " layout "  pitch " pitch)
    (map-physical-memory (logand phys (lognot #xFFF))
                         (logand (+ (* height pitch) (logand phys #xFFF) #xFFF) (lognot #xFFF))
                         "System Framebuffer")
    (setf *current-framebuffer* (make-framebuffer :base-address phys
                                                  :width width
                                                  :height height
                                                  :pitch pitch
                                                  :layout layout))))

(defun current-framebuffer ()
  *current-framebuffer*)

(defun framebuffer-blit (fb nrows ncols from-array from-row from-col to-row to-col)
  "Update a region of the system framebuffer.
Returns false if the framebuffer is invalid, true otherwise.
If the framebuffer is invalid, the caller should fetch the current framebuffer and discard the old one."
  (when (not (eql (array-rank from-array) 2))
    (error 'type-error
           :expected-type '(array (unsigned-byte 32) (* *))
           :datum from-array))
  ;; Don't write to the top row of pixels, that's where the lights are.
  (when (<= to-row 0)
    (incf from-row (- 1 to-row))
    (decf nrows (- 1 to-row))
    (setf to-row 1))
  ;; Dismember the from-array.
  (let ((from-offset 0)
        (from-storage (sys.int::%complex-array-storage from-array))
        (from-width (array-dimension from-array 1))
        (from-height (array-dimension from-array 0)))
    ;; Check for displaced arrays.
    (when (integerp (sys.int::%complex-array-info from-array))
      (setf from-offset (sys.int::%complex-array-info from-array)
            from-storage (sys.int::%complex-array-storage from-storage)))
    ;; Storage must be a simple ub32 array, not a fixnum (memory array)
    (when (or (sys.int::fixnump from-storage)
              (not (eql (sys.int::%object-tag from-storage)
                        sys.int::+object-tag-array-unsigned-byte-32+)))
      (error 'type-error
             :expected-type '(and (array (unsigned-byte 32) (* *))
                                  (not sys.int::memory-array))
             :datum from-array))
    ;; Clamp parameters.
    ;; Only need to clamp values below zero here. nrows/ncols will
    ;; end up negative if the source/target positions are too large.
    ;; Clamp to row/column.
    (when (< to-row 0)
      (incf nrows to-row)
      (decf from-row to-row)
      (setf to-row 0))
    (when (< to-col 0)
      (incf ncols to-col)
      (decf from-col to-col)
      (setf to-col 0))
    ;; Clamp from row/column.
    (when (< from-row 0)
      (incf nrows from-row)
      (decf to-row from-row)
      (setf from-row 0))
    (when (< from-col 0)
      (incf ncols from-col)
      (decf to-col from-col)
      (setf from-col 0))
    ;; Clamp nrows/ncols.
    (setf nrows (max 0 (min nrows (- (framebuffer-height fb) to-row) (- from-height from-row))))
    (setf ncols (max 0 (min ncols (- (framebuffer-width fb) to-col) (- from-width from-col))))
    ;; Disable snapshotting and the GC while this is in progress, as we're touching physical memory.
    (with-pseudo-atomic
      (when (not (eql fb *current-framebuffer*))
        (return-from framebuffer-blit nil))
      (let ((to-base (+ +physical-map-base+
                        (framebuffer-base-address fb)
                        (* to-row (framebuffer-pitch fb))
                        (* to-col 4)))
            (to-pitch (framebuffer-pitch fb))
            (from-base (+ (sys.int::lisp-object-address from-storage) (- sys.int::+tag-object+) 8
                          (* from-row from-width 4)
                          (* from-col 4)))
            (from-pitch (* from-width 4)))
        (dotimes (i nrows)
          (%%bitblt-row to-base from-base ncols)
          (incf to-base to-pitch)
          (incf from-base from-pitch))))
    t))

;; (to-base from-base ncols)
(sys.int::define-lap-function %%bitblt-row ()
  (sys.lap-x86:mov64 :rdi :r8) ; to-storage
  (sys.lap-x86:mov64 :rsi :r9) ; from-storage
  (sys.lap-x86:mov64 :rcx :r10) ; ncols
  (sys.lap-x86:sar64 :rsi #.sys.int::+n-fixnum-bits+)
  (sys.lap-x86:sar64 :rdi #.sys.int::+n-fixnum-bits+)
  (sys.lap-x86:sar64 :rcx #.sys.int::+n-fixnum-bits+)
  (sys.lap-x86:rep)
  (sys.lap-x86:movs32)
  (sys.lap-x86:ret))

;; (to-base colour ncols)
(sys.int::define-lap-function %%bitset-row ()
  (sys.lap-x86:mov64 :rdi :r8) ; to-storage
  (sys.lap-x86:mov64 :rax :r9) ; colour
  (sys.lap-x86:mov64 :rcx :r10) ; ncols
  (sys.lap-x86:sar64 :rdi #.sys.int::+n-fixnum-bits+)
  (sys.lap-x86:sar64 :rax #.sys.int::+n-fixnum-bits+)
  (sys.lap-x86:sar64 :rcx #.sys.int::+n-fixnum-bits+)
  (sys.lap-x86:rep)
  (sys.lap-x86:stos32)
  (sys.lap-x86:ret))

(defun set-panic-light ()
  (when *current-framebuffer*
    (let ((fb-addr (+ +physical-map-base+
                      (framebuffer-base-address *current-framebuffer*))))
      (dotimes (i (framebuffer-width *current-framebuffer*))
        (setf (sys.int::memref-unsigned-byte-32 fb-addr i) #xFFFF0000)))))

(defmacro deflight (name colour position)
  (let ((setter (intern (format nil "SET-~A-LIGHT" name))))
    `(defun ,setter (state)
       (safe-without-interrupts (state)
         (when *current-framebuffer*
           (let ((fb-addr (+ +physical-map-base+
                             (framebuffer-base-address *current-framebuffer*)
                             (* ,position 32 4))))
             (dotimes (i 32)
               (setf (sys.int::memref-unsigned-byte-32 fb-addr i) (if state
                                                                      ,colour
                                                                      0)))))))))

(deflight disk-read #xFF00FF00 0)
(deflight disk-write #xFFFF0000 1)
(deflight gc #xFFFF00FF 2)
(deflight run #xFF00FFFF 3)
(deflight snapshot #xFFFFFF00 4)
(deflight paging #xFFFF8000 5)

(defvar sys.int::*debug-8x8-font*)

(defun debug-video-write-char (char)
  (cond ((not *current-framebuffer*))
        ((eql char #\Newline)
         (incf *debug-video-y* 8)
         (setf *debug-video-x* 0)
         (when (>= *debug-video-y* (framebuffer-height *current-framebuffer*))
           (setf *debug-video-y* 0)
           (incf *debug-video-col*)
           (when (>= *debug-video-col* 3)
             (setf *debug-video-col* 0)))
         (let* ((fb *current-framebuffer*)
                (stride (framebuffer-pitch fb))
                (col-width (truncate (framebuffer-width fb) 3))
                (addr (+ +physical-map-base+
                         (framebuffer-base-address fb)
                         (* *debug-video-col* col-width 4)
                         (* *debug-video-y* stride))))
           (dotimes (i 8)
             (%%bitset-row addr #xFFFFFFFF col-width)
             (incf addr stride))))
        (t
         (let ((code (char-code char)))
           (when (< code 128)
             (let* ((blob (svref sys.int::*debug-8x8-font* code))
                    (fb *current-framebuffer*)
                    (col-width (truncate (framebuffer-width fb) 3))
                    (to-base (+ +physical-map-base+
                                (framebuffer-base-address fb)
                                (* *debug-video-y* (framebuffer-pitch fb))
                                (* *debug-video-x* 4)
                                (* *debug-video-col* col-width 4)))
                    (to-pitch (framebuffer-pitch fb))
                    (from-base (+ (sys.int::lisp-object-address blob) (- sys.int::+tag-object+) 8))
                    (from-pitch (* 8 4)))
               (dotimes (i 8)
                 (%%bitblt-row to-base from-base 8)
                 (incf to-base to-pitch)
                 (incf from-base from-pitch))
               (incf *debug-video-x* 8)
               (when (>= *debug-video-x* col-width)
                 (debug-video-write-char #\Newline))))))))

(defun debug-video-write-string (string)
  (dotimes (i (string-length string))
    (debug-video-write-char (char string i))))

(defun debug-video-stream (op &optional arg)
  (ecase op
    (:read-char (loop))
    (:clear-input)
    (:write-char (debug-video-write-char arg))
    (:write-string (debug-video-write-string arg))
    (:force-output)
    (:start-line-p (eql *debug-video-x* 0))))
