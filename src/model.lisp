(in-package #:bootwright)

(defstruct (bios-mbr-protocol
            (:constructor make-bios-mbr-protocol
                (&key load-address sector-size kernel-segment signature)))
  (load-address #x7C00)
  (sector-size 512)
  (kernel-segment #x1000)
  (signature #xAA55))

(defstruct (memory-region
            (:constructor make-memory-region (&key name start end type)))
  name
  start
  end
  type)

(defstruct (pic8259-controller
            (:constructor make-pic8259-controller
                (&key master-base slave-base master-mask-port slave-mask-port
                      vector-base slave-identity)))
  (master-base #x20)
  (slave-base #xA0)
  (master-mask-port #x21)
  (slave-mask-port #xA1)
  (vector-base 32)
  (slave-identity 2))

(defstruct (pit8253-timer
            (:constructor make-pit8253-timer
                (&key channel0-port channel1-port channel2-port control-port
                      irq base-frequency)))
  (channel0-port #x40)
  (channel1-port #x41)
  (channel2-port #x42)
  (control-port #x43)
  (irq 0)
  (base-frequency 1193182))

(defstruct (uart16550-device
            (:constructor make-uart16550-device
                (&key name base-port irq baud divisor)))
  name
  (base-port #x3F8)
  (irq 4)
  (baud 115200)
  (divisor 1))

(defstruct (vga-text-framebuffer
            (:constructor make-vga-text-framebuffer
                (&key name columns rows base-address default-attribute)))
  name
  (columns 80)
  (rows 25)
  (base-address #xB8000)
  (default-attribute #x07))

(defstruct (machine-descriptor
            (:constructor make-machine-descriptor
                (&key name isa boot-protocol memory-regions interrupt-controller
                      timer-device serial-devices framebuffer-devices)))
  name
  isa
  boot-protocol
  memory-regions
  interrupt-controller
  timer-device
  serial-devices
  framebuffer-devices)

(defstruct (image-spec (:constructor make-image-spec (&key name target sections source-pathname)))
  name
  target
  sections
  source-pathname)

(defstruct (section-spec
            (:constructor make-section-spec
                (&key name kind body entry load-segment load-offset origin sector-align source-root)))
  name
  kind
  body
  entry
  load-segment
  (load-offset 0)
  origin
  (sector-align 512)
  source-root)

(defstruct (compiled-section
            (:constructor make-compiled-section
                (&key spec bytes origin labels entry-address)))
  spec
  bytes
  origin
  labels
  entry-address)

(defstruct (section-layout
            (:constructor make-section-layout
                (&key name kind start-lba sector-count byte-count
                      load-segment load-offset origin entry-offset)))
  name
  kind
  start-lba
  sector-count
  byte-count
  load-segment
  load-offset
  origin
  entry-offset)

(defstruct (compiled-image
            (:constructor make-compiled-image (&key name target bytes layouts)))
  name
  target
  bytes
  layouts)

(defun boot-protocol-load-address (protocol)
  (typecase protocol
    (bios-mbr-protocol
     (bios-mbr-protocol-load-address protocol))
    (t
     (error "Unsupported boot protocol ~S." protocol))))

(defun boot-protocol-sector-size (protocol)
  (typecase protocol
    (bios-mbr-protocol
     (bios-mbr-protocol-sector-size protocol))
    (t
     (error "Unsupported boot protocol ~S." protocol))))

(defun boot-protocol-kernel-segment (protocol)
  (typecase protocol
    (bios-mbr-protocol
     (bios-mbr-protocol-kernel-segment protocol))
    (t
     (error "Unsupported boot protocol ~S." protocol))))

(defun make-compatibility-pc-machine (&key (name :x86-bios)
                                           (boot-origin #x7C00)
                                           (sector-size 512)
                                           (kernel-segment #x1000))
  (make-machine-descriptor
   :name name
   :isa :x86-32
   :boot-protocol (make-bios-mbr-protocol :load-address boot-origin
                                          :sector-size sector-size
                                          :kernel-segment kernel-segment
                                          :signature #xAA55)
   :memory-regions (list (make-memory-region :name :conventional
                                             :start #x00000
                                             :end #x9FFFF
                                             :type :ram)
                         (make-memory-region :name :vga-text
                                             :start #xB8000
                                             :end #xBFFFF
                                             :type :mmio)
                         (make-memory-region :name :bios-rom
                                             :start #xF0000
                                             :end #xFFFFF
                                             :type :rom))
   :interrupt-controller (make-pic8259-controller :vector-base 32)
   :timer-device (make-pit8253-timer :irq 0)
   :serial-devices (list (make-uart16550-device :name :com1
                                                :base-port #x3F8
                                                :irq 4))
   :framebuffer-devices (list (make-vga-text-framebuffer :name :screen0
                                                         :columns 80
                                                         :rows 25
                                                         :base-address #xB8000
                                                         :default-attribute #x07))))

(defun normalize-device-name (designator)
  (etypecase designator
    (symbol (token-keyword designator))
    (string (token-keyword designator))))

(defun find-named-device (devices accessor designator kind)
  (let* ((name (normalize-device-name designator))
         (device (find name devices
                       :key accessor
                       :test #'equal)))
    (or device
        (error "Unknown ~A device ~S." kind designator))))

(defun find-machine-serial-device (machine designator)
  (find-named-device (machine-descriptor-serial-devices machine)
                     #'uart16550-device-name
                     designator
                     "serial"))

(defun find-machine-framebuffer (machine designator)
  (find-named-device (machine-descriptor-framebuffer-devices machine)
                     #'vga-text-framebuffer-name
                     designator
                     "framebuffer"))

(defun section-linear-base (section)
  (or (and (section-spec-load-segment section)
           (+ (* 16 (section-spec-load-segment section))
              (section-spec-load-offset section)))
      (section-spec-origin section)
      0))

(defun normalize-target (designator)
  (typecase designator
    (machine-descriptor
     designator)
    (symbol
     (or (find-machine designator)
         (error "Unknown machine designator ~S." designator)))
    (cons
     (destructuring-bind (kind &rest options) designator
       (ecase kind
         (:x86-bios
          (make-compatibility-pc-machine :name :x86-bios
                                         :boot-origin (getf options :boot-origin #x7C00)
                                         :sector-size (getf options :sector-size 512)
                                         :kernel-segment (getf options :kernel-segment #x1000))))))
    (t
     (error "Unsupported target designator ~S." designator))))

(defun section-origin (section)
  (or (section-spec-origin section)
      (and (section-spec-load-segment section)
           (section-spec-load-offset section))
      0))

(defun parse-section-form (form target source-root)
  (let ((protocol (machine-descriptor-boot-protocol target)))
  (case (token-keyword (first form))
    (:boot-sector
     (destructuring-bind (options &body body) (rest form)
       (make-section-spec :name :boot
                          :kind :boot
                          :body body
                          :entry (getf options :entry)
                          :origin (boot-protocol-load-address protocol)
                          :sector-align (boot-protocol-sector-size protocol)
                          :source-root source-root)))
    (:kernel-section
     (destructuring-bind (name options &body body) (rest form)
       (make-section-spec :name name
                          :kind :kernel
                          :body body
                          :entry (getf options :entry)
                          :load-segment (getf options :load-segment
                                              (boot-protocol-kernel-segment protocol))
                          :load-offset (getf options :load-offset 0)
                          :origin (getf options :origin)
                          :sector-align (getf options :sector-align
                                              (boot-protocol-sector-size protocol))
                          :source-root source-root)))
    (:section
     (destructuring-bind (name options &body body) (rest form)
       (make-section-spec :name name
                          :kind (getf options :kind :data)
                          :body body
                          :entry (getf options :entry)
                          :load-segment (getf options :load-segment)
                          :load-offset (getf options :load-offset 0)
                          :origin (getf options :origin)
                          :sector-align (getf options :sector-align
                                              (boot-protocol-sector-size protocol))
                          :source-root source-root)))
    (t
     (error "Unknown section form ~S." (first form))))))

(defun parse-sections (section-forms target &optional source-root)
  (mapcar (lambda (form) (parse-section-form form target source-root))
          section-forms))

(defun find-section-layout (layouts name)
  (or (find name layouts :key #'section-layout-name :test #'equal)
      (error "Unknown section ~S." name)))
