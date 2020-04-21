(defpackage :mezzano.gui.virgl
  (:use :cl)
  (:local-nicknames (:gpu :mezzano.supervisor.virtio-gpu)
                    (:ext :mezzano.extensions)
                    (:sup :mezzano.supervisor)))

(defpackage :mezzano.gui.virgl.tgsi
  (:use :cl)
  (:export #:assemble
           #:dcl #:imm
           #:mov #:end #:sub))
