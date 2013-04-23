;;;; Definitions and helper functions for KBoot.

(in-package :sys.int)

(defmacro sys.int::define-constant (name initial-value &key (test ''eql) documentation)
  `(defconstant ,name (let ((val ,initial-value))
                        (if (and (boundp ',name)
                                 (funcall ,test val (symbol-value ',name)))
                            (symbol-value ',name)
                            val))
     ,@(when documentation `(,documentation))))

;;; KBoot tag list tag types.
(defconstant +kboot-tag-none+ 0
  "Tag marking the end of the tag list.")
(defconstant +kboot-tag-core+ 1
  "Tag describing the boot state.")
(defconstant +kboot-tag-option+ 2
  "Tag detailing a kernel option.")
(defconstant +kboot-tag-memory+ 3
  "Tag describing a physical memory range.")
(defconstant +kboot-tag-vmem+ 4
  "Tag describing a boot pagetable mapping.")
(defconstant +kboot-tag-pagetables+ 5
  "Platform-specific tag describing boot pagetables.")
(defconstant +kboot-tag-module+ 6
  "Tag describing a loaded module.")
(defconstant +kboot-tag-video+ 7
  "Tag describing the current video mode.")
(defconstant +kboot-tag-bootdev+ 8
  "Tag describing the boot device.")
(defconstant +kboot-tag-log+ 9
  "Tag describing the crash log.")
(defconstant +kboot-tag-sections+ 10
  "Tag describing addtional ELF sections that were loaded.")
(defconstant +kboot-tag-e820+ 11
  "Tag describing one E820 memory map entry.")

;;; MEMORY types.
(defconstant +kboot-memory-free+ 0
  "Free memory available for the kernel to use.")
(defconstant +kboot-memory-allocated+ 1
  "Memory allocated by the boot loader to conain the kernel image, stack, or other data.")
(defconstant +kboot-memory-reclaimable+ 2
  "Memory that currently contains KBoot data.")
(defconstant +kboot-memory-pagetables+ 3
  "Memory that currently contains the KBoot boot pagetables.")
(defconstant +kboot-memory-stack+ 4
  "Memory that currently contains KBoot boot stack.")
(defconstant +kboot-memory-modules+ 5
  "Memory that currently contains KBoot modules.")

;;; VIDEO mode, also used by the kernel to signal supported modes.
(defconstant +kboot-video-vga+ (ash 1 0))
(defconstant +kboot-video-lfb+ (ash 1 1))
;;; LFB modes.
(defconstant +kboot-lfb-rgb+ (ash 1 0))
(defconstant +kboot-lfb-indexed+ (ash 1 1))

;;; Boot device.
(defconstant +kboot-bootdev-none+ 0
  "No boot device.")
(defconstant +kboot-bootdev-disk+ 1
  "Booted from a mass storage device.")
(defconstant +kboot-bootdev-network+ 2
  "Booted from the network")

;;; Network mode.
(defconstant +kboot-net-ipv6+ (ash 1 0))

(defvar *kboot-tag-names*
  #("NONE"
    "CORE"
    "OPTION"
    "MEMORY"
    "VMEM"
    "PAGETABLES"
    "MODULE"
    "VIDEO"
    "BOOTDEV"
    "LOG"
    "SECTIONS"
    "E820")
  "Friendly name of the tags.")

(define-constant +kboot-note-name+ #(#x4B #x42 #x6F #x6F #x74 #x00) ; "KBoot\0"
  :test 'equalp
  :documentation "The name used by KBoot ELF notes.")
(defconstant +kboot-itag-image+ 0
  "Note marking a KBoot image.")
(defconstant +kboot-itag-image-length+ 8
  "Length in bytes of an IMAGE note.")
(defconstant +kboot-itag-load+ 1
  "Note describing the kernel memory layout.")
(defconstant +kboot-itag-load-length+ 48
  "Length in bytes of a LOAD note.")
(defconstant +kboot-itag-option+ 2
  "Note describing a bootloader option.")
(defconstant +kboot-itag-option-length+ 13
  "Length in bytes of an OPTION note.")
(defconstant +kboot-itag-mapping+ 3
  "Note describing an additional virtual memory mapping.")
(defconstant +kboot-itag-mapping-length+ 24
  "Length in bytes of a MAPPING note.")
(defconstant +kboot-itag-video+ 4
  "Note describing kernel video mode support.")
(defconstant +kboot-itag-video-length+ 13
  "Length in bytes of a VIDEO note.")

(defconstant +kboot-image-version+ 1 "Current KBoot header version.")
(defconstant +kboot-image-sections+ (ash 1 0)
  "Flag bit requesting that additional ELF sections be loaded.")
(defconstant +kboot-image-log+ (ash 1 1)
  "Flag bit requesting kernel crash log support.")
(defconstant +kboot-load-fixed+ (ash 1 0)
  "Flag bit requiring a fixed memory layout.")
(defconstant +kboot-option-boolean+ 0
  "A boolean option.")
(defconstant +kboot-option-string+ 1
  "A string option.")
(defconstant +kboot-option-integer+ 2
  "An integer option.")

;;; ELF64 stuff.
(define-constant +elf64-ident+ #(#x7F #x45 #x4C #x46 ; Magic, "\x7FELF".
                                 #x02                ; 64-bit class.
                                 #x01                ; LSB data.
                                 #x01                ; ELF v1.
                                 #xFF                ; OS ABI, standalone.
                                 #x00                ; ABI version.
                                 ;; Padding.
                                 #x00 #x00 #x00 #x00 #x00 #x00 #x00)
  :test 'equalp
  :documentation "ELF64 e_ident field.")

(defconstant +elf-header-size+ 64 "Size of ELF header in octets.")
(defconstant +elf-e_ident+ 0 "Offset of e_ident field.")
(defconstant +elf-e_type+ 16 "Offset of e_type field.")
(defconstant +elf-e_machine+ 18 "Offset of e_machine field.")
(defconstant +elf-e_version+ 20 "Offset of e_version field.")
(defconstant +elf-e_entry+ 24 "Offset of e_entry field.")
(defconstant +elf-e_phoff+ 32 "Offset of e_phoff field.")
(defconstant +elf-e_shoff+ 40 "Offset of e_shoff field.")
(defconstant +elf-e_flags+ 48 "Offset of e_flags field.")
(defconstant +elf-e_ehsize+ 52 "Offset of e_ehsize field.")
(defconstant +elf-e_phentsize+ 54 "Offset of e_phentsize field.")
(defconstant +elf-e_phnum+ 56 "Offset of e_phnum field.")
(defconstant +elf-e_shentsize+ 58 "Offset of e_shentsize field.")
(defconstant +elf-e_shnum+ 60 "Offset of e_shnum field.")
(defconstant +elf-e_shstrndx+ 62 "Offset of e_shstrndx field.")
(defconstant +elf-type-exec+ 2 "ELF e_type field.")
(defconstant +elf-machine-x86-64+ 62 "ELF e_machine field.")
(defconstant +elf-version+ 1 "ELF e_version field.")

(defconstant +elf-phdr-size+ 56 "Size of one ELF program header in octets.")
(defconstant +elf-p_type+ 0 "Offset of p_type field.")
(defconstant +elf-p_flags+ 4 "Offset of p_flags field.")
(defconstant +elf-p_offset+ 8 "Offset of p_offset field.")
(defconstant +elf-p_vaddr+ 16 "Offset of p_vaddr field.")
(defconstant +elf-p_paddr+ 24 "Offset of p_paddr field.")
(defconstant +elf-p_filesz+ 32 "Offset of p_filesz field.")
(defconstant +elf-p_memsz+ 40 "Offset of p_memsz field.")
(defconstant +elf-p_align+ 48 "Offset of p_align field.")

(defconstant +elf-shdr-size+ 64 "Size of one ELF section header in octets.")

(defconstant +elf-pt-null+ 0 "NULL phdr type.")
(defconstant +elf-pt-load+ 1 "LOAD phdr type.")
(defconstant +elf-pt-note+ 4 "NOTE phdr type.")

(defun elf-note (name type data)
  "Construct an ELF note."
  ;; +0  4B Length of name.
  ;; +4  4B Length of data.
  ;; +8  4B Type
  ;; +12 x  Name, padded up to 4 bytes.
  ;; +12+x  Data, padded up to 4 bytes.
  (let* ((len (+ 4 4 4 (round-up (length name) 4) (round-up (length data) 4)))
         (vec (make-array len
                          :element-type '(unsigned-byte 8)
                          :initial-element 0)))
    (setf (ub32ref/le vec 0) (length name)
          (ub32ref/le vec 4) (length data)
          (ub32ref/le vec 8) type)
    (replace vec name :start1 12)
    (replace vec data :start1 (+ 12 (round-up (length name) 4)))
    vec))

(defun kboot-itag-image (&key crash-log load-sections)
  "Build a KBoot IMAGE note."
  (let ((vec (make-array +kboot-itag-image-length+
                         :element-type '(unsigned-byte 8)
                         :initial-element 0)))
    (setf (ub32ref/le vec 0) +kboot-image-version+
          (ub32ref/le vec 4) (logior (if load-sections +kboot-image-sections+ 0)
                                     (if crash-log +kboot-image-log+ 0)))
    (elf-note +kboot-note-name+ +kboot-itag-image+ vec)))

(defun kboot-itag-load ()
  "Build a KBoot LOAD note."
  (let ((vec (make-array +kboot-itag-load-length+
                         :element-type '(unsigned-byte 8)
                         :initial-element 0)))
    (setf (ub32ref/le vec 0) (logior +kboot-load-fixed+))
    (elf-note +kboot-note-name+ +kboot-itag-load+ vec)))

(defun kboot-itag-video (&key (vga t) (lfb t))
  "Build a KBoot VIDEO note."
  (let ((vec (make-array +kboot-itag-video-length+
                         :element-type '(unsigned-byte 8)
                         :initial-element 0)))
    (setf (ub32ref/le vec 0) (logior (if vga +kboot-video-vga+ 0)
                                     (if lfb +kboot-video-lfb+ 0)))
    (elf-note +kboot-note-name+ +kboot-itag-video+ vec)))

(defun make-elf-header (entry phnum &optional (phoff +elf-header-size+))
  "Build an ELF header."
  (let ((elf-header (make-array +elf-header-size+
                                :element-type '(unsigned-byte 8)
                                :initial-element 0)))
    (replace elf-header +elf64-ident+)
    (setf (ub16ref/le elf-header +elf-e_type+) +elf-type-exec+
          (ub16ref/le elf-header +elf-e_machine+) +elf-machine-x86-64+
          (ub32ref/le elf-header +elf-e_version+) +elf-version+
          (ub64ref/le elf-header +elf-e_entry+) entry
          ;; Program headers come after the ELF header.
          (ub64ref/le elf-header +elf-e_phoff+) phoff
          ;; No section headers.
          (ub64ref/le elf-header +elf-e_shoff+) 0
          (ub32ref/le elf-header +elf-e_flags+) 0
          (ub16ref/le elf-header +elf-e_ehsize+) +elf-header-size+
          (ub16ref/le elf-header +elf-e_phentsize+) +elf-phdr-size+
          (ub16ref/le elf-header +elf-e_phnum+) phnum
          (ub16ref/le elf-header +elf-e_shentsize+) +elf-shdr-size+
          (ub16ref/le elf-header +elf-e_shnum+) 0
          (ub16ref/le elf-header +elf-e_shstrndx+) 0)
    elf-header))

(defun make-elf-phdr (type &key (offset 0) (vaddr 0) (paddr vaddr) (filesz 0) (memsz filesz))
  (let ((phdr (make-array +elf-phdr-size+
                          :element-type '(unsigned-byte 8)
                          :initial-element 0)))
    (setf (ub32ref/le phdr +elf-p_type+) type
          (ub32ref/le phdr +elf-p_flags+) #b111 ; RWX
          (ub64ref/le phdr +elf-p_offset+) offset
          (ub64ref/le phdr +elf-p_vaddr+) vaddr
          (ub64ref/le phdr +elf-p_paddr+) paddr
          (ub64ref/le phdr +elf-p_filesz+) filesz
          (ub64ref/le phdr +elf-p_memsz+) memsz
          (ub64ref/le phdr +elf-p_align+) 4)
    phdr))

(defun make-kboot-header (kboot-entry load-addr file-size mem-size round-to &key (vga t) (lfb t))
  "Build a complete ELF/KBoot header, assuming that the kernel is stored
immediately after the header."
  (let ((notes '())
        n-phdrs
        file-offset
        hdr)
    ;; Add notes.
    (push (list (kboot-itag-image) 0) notes)
    (push (list (kboot-itag-load) 0) notes)
    (push (list (kboot-itag-video :vga vga :lfb lfb) 0) notes)
    ;; LOAD phdr plus NOTE phdrs.
    (setf n-phdrs (+ 1 (length notes)))
    ;; Compute note file offsets.
    (setf file-offset (+ +elf-header-size+ (* n-phdrs +elf-phdr-size+)))
    (dolist (n notes)
      (setf (second n) file-offset)
      (incf file-offset (length (first n))))
    ;; Align file offset.
    (setf file-offset (round-up file-offset round-to))
    (setf hdr (make-array file-offset
                          :element-type '(unsigned-byte 8)
                          :initial-element 0))
    ;; Write out the ELF header.
    (replace hdr (make-elf-header kboot-entry n-phdrs) :start1 0)
    ;; Write program headers.
    (replace hdr (make-elf-phdr +elf-pt-load+
                                :offset file-offset
                                :vaddr load-addr
                                :filesz file-size
                                :memsz mem-size)
             :start1 +elf-header-size+)
    (loop
       for i from 1
       for (data offset) in notes
       do (replace hdr (make-elf-phdr +elf-pt-note+
                                      :offset offset
                                      :filesz (length data))
                   :start1 (+ +elf-header-size+
                              (* i +elf-phdr-size+))))
    ;; Write notes.
    (loop for (data offset) in notes do
         (replace hdr data :start1 offset))
    hdr))

(defun kboot-mmap-memory-type (type)
  "Convert a multiboot memory map type to a normal memory type."
  (case type
    (#.+kboot-memory-free+ :free)
    (#.+kboot-memory-allocated+ :reserved)
    (#.+kboot-memory-reclaimable+ :kboot-reclaimable)
    (#.+kboot-memory-pagetables+ :free)
    (#.+kboot-memory-stack+ :free)
    (#.+kboot-memory-modules+ :kboot-modules)
    (t :unknown)))

(defun kboot-memory-map ()
  "Fetch the memory map from the KBoot tag list."
  (flet ((p/8 (addr) (memref-unsigned-byte-8 (+ #x8000000000 addr) 0))
         (p/16 (addr) (memref-unsigned-byte-16 (+ #x8000000000 addr) 0))
         (p/32 (addr) (memref-unsigned-byte-32 (+ #x8000000000 addr) 0))
         (p/64 (addr) (memref-unsigned-byte-64 (+ #x8000000000 addr) 0)))
    (let ((addr *kboot-tag-list*)
          (mmap (make-array 32 :adjustable t :fill-pointer 0))
          ;; For sanity checking.
          (max-addr (+ *kboot-tag-list* 4096))
          (best-start nil)
          (best-size 0))
      (loop (when (>= addr max-addr) (return))
         (let ((type (p/32 (+ addr 0)))
               (size (p/32 (+ addr 4))))
           (when (and (eql addr *kboot-tag-list*)
                      (not (eql type +kboot-tag-core+)))
             (format t "CORE tag not first in the list?~%")
             (return))
           (case type
             (#.+kboot-tag-none+ (return))
             (#.+kboot-tag-core+
              (unless (eql addr *kboot-tag-list*)
                (format t "CORE tag not first in the list?~%")
                (return))
              (setf max-addr (+ *kboot-tag-list* (p/32 (+ addr 16)))))
             (#.+kboot-tag-memory+
              (let ((start (p/64 (+ addr 8)))
                    (length (p/64 (+ addr 16)))
                    (type (p/8 (+ addr 24))))
                (vector-push-extend (make-memory-map-entry
                                     :base start
                                     :length length
                                     :type (kboot-mmap-memory-type type))
                                    mmap))))
           (incf addr (round-up size 8))))
      mmap)))
