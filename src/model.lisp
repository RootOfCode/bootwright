(in-package #:bootwright)

(defstruct (bios-target
            (:constructor make-bios-target (&key boot-origin sector-size kernel-segment)))
  (boot-origin #x7C00)
  (sector-size 512)
  (kernel-segment #x1000))

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

(defun section-linear-base (section)
  (or (and (section-spec-load-segment section)
           (+ (* 16 (section-spec-load-segment section))
              (section-spec-load-offset section)))
      (section-spec-origin section)
      0))

(defun normalize-target (designator)
  (typecase designator
    (bios-target
     designator)
    (cons
     (destructuring-bind (kind &rest options) designator
       (ecase kind
         (:x86-bios
          (make-bios-target :boot-origin (getf options :boot-origin #x7C00)
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
  (case (token-keyword (first form))
    (:boot-sector
     (destructuring-bind (options &body body) (rest form)
       (make-section-spec :name :boot
                          :kind :boot
                          :body body
                          :entry (getf options :entry)
                          :origin (bios-target-boot-origin target)
                          :sector-align (bios-target-sector-size target)
                          :source-root source-root)))
    (:kernel-section
     (destructuring-bind (name options &body body) (rest form)
       (make-section-spec :name name
                          :kind :kernel
                          :body body
                          :entry (getf options :entry)
                          :load-segment (getf options :load-segment
                                              (bios-target-kernel-segment target))
                          :load-offset (getf options :load-offset 0)
                          :origin (getf options :origin)
                          :sector-align (getf options :sector-align
                                              (bios-target-sector-size target))
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
                                              (bios-target-sector-size target))
                          :source-root source-root)))
    (t
     (error "Unknown section form ~S." (first form)))))

(defun parse-sections (section-forms target &optional source-root)
  (mapcar (lambda (form) (parse-section-form form target source-root))
          section-forms))

(defun find-section-layout (layouts name)
  (or (find name layouts :key #'section-layout-name :test #'equal)
      (error "Unknown section ~S." name)))
