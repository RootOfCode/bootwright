(in-package #:bootwright)

(defun make-zero-padded-copy (bytes total-size)
  (let ((buffer (make-octet-buffer)))
    (append-octets buffer bytes)
    (loop repeat (- total-size (length bytes)) do
      (append-octet buffer 0))
    buffer))

(defgeneric finalize-image (protocol compiled-boot payload-sections))

(defun finalize-boot-sector (compiled-section protocol)
  (let* ((bytes (compiled-section-bytes compiled-section))
         (sector-size (boot-protocol-sector-size protocol))
         (limit (- sector-size 2)))
    (when (> (length bytes) limit)
      (error "Boot sector is ~D bytes, but only ~D bytes are available before the boot signature."
             (length bytes)
             limit))
    (let ((buffer (make-zero-padded-copy bytes limit)))
      (append-octet buffer (ldb (byte 8 0) (bios-mbr-protocol-signature protocol)))
      (append-octet buffer (ldb (byte 8 8) (bios-mbr-protocol-signature protocol)))
      buffer)))

(defmethod finalize-image ((protocol bios-mbr-protocol) compiled-boot payload-sections)
  (let ((buffer (make-octet-buffer)))
    (append-octets buffer (finalize-boot-sector compiled-boot protocol))
    (dolist (compiled payload-sections)
      (append-padded-section buffer (compiled-section-bytes compiled) protocol))
    buffer))

(defun compute-layouts (compiled-sections protocol)
  (let ((current-lba 1)
        (sector-size (boot-protocol-sector-size protocol)))
    (mapcar (lambda (compiled)
              (let* ((spec (compiled-section-spec compiled))
                     (byte-count (length (compiled-section-bytes compiled)))
                     (sector-count (max 1 (ceiling-div byte-count sector-size)))
                     (layout (make-section-layout
                              :name (section-spec-name spec)
                              :kind (section-spec-kind spec)
                              :start-lba current-lba
                              :sector-count sector-count
                              :byte-count byte-count
                              :load-segment (section-spec-load-segment spec)
                              :load-offset (section-spec-load-offset spec)
                              :origin (compiled-section-origin compiled)
                              :entry-offset (if (compiled-section-entry-address compiled)
                                                (- (compiled-section-entry-address compiled)
                                                   (compiled-section-origin compiled))
                                                0))))
                (incf current-lba sector-count)
                layout))
            compiled-sections)))

(defun append-padded-section (buffer bytes protocol)
  (let* ((sector-size (boot-protocol-sector-size protocol))
         (padded-size (align-up (length bytes) sector-size)))
    (append-octets buffer bytes)
    (loop repeat (- padded-size (length bytes)) do
      (append-octet buffer 0)))
  buffer)

(defun normalize-image-spec (designator)
  (typecase designator
    (image-spec designator)
    (symbol
     (let ((value (and (boundp designator) (symbol-value designator))))
       (unless (typep value 'image-spec)
         (error "Symbol ~S does not name a Bootwright image spec." designator))
       value))
    (t
     (error "Cannot build image from ~S." designator))))

(defun build-image (designator)
  (let* ((spec (normalize-image-spec designator))
         (target (normalize-target (image-spec-target spec)))
         (protocol (machine-descriptor-boot-protocol target))
         (source-root (and (image-spec-source-pathname spec)
                           (pathname-directory-pathname (image-spec-source-pathname spec))))
         (sections (parse-sections (image-spec-sections spec) target source-root))
         (boot (find :boot sections :key #'section-spec-kind))
         (payload (remove :boot sections :key #'section-spec-kind)))
    (unless boot
      (error "Image ~S does not define a boot sector." (image-spec-name spec)))
    (let* ((compiled-payload (mapcar (lambda (section)
                                       (compile-section-spec section target '()))
                                     payload))
           (layouts (compute-layouts compiled-payload protocol))
           (compiled-boot (compile-section-spec boot target layouts))
           (buffer (finalize-image protocol compiled-boot compiled-payload)))
      (make-compiled-image :name (image-spec-name spec)
                           :target target
                           :bytes buffer
                           :layouts layouts))))

(defun write-image (compiled-image pathname)
  (write-octets-to-file (compiled-image-bytes compiled-image) pathname))

(defun build-image-file (designator pathname)
  (let ((compiled (build-image designator)))
    (write-image compiled pathname)
    pathname))

(defun describe-image (compiled-image &optional (stream *standard-output*))
  (format stream "~&Image ~S~%" (compiled-image-name compiled-image))
  (format stream "  size: ~D bytes~%" (length (compiled-image-bytes compiled-image)))
  (dolist (layout (compiled-image-layouts compiled-image))
    (format stream "  section ~S: lba=~D sectors=~D bytes=~D load=~:[n/a~;~4,'0X:~4,'0X~]~%"
            (section-layout-name layout)
            (section-layout-start-lba layout)
            (section-layout-sector-count layout)
            (section-layout-byte-count layout)
            (section-layout-load-segment layout)
            (section-layout-load-segment layout)
            (section-layout-load-offset layout)))
  compiled-image)
