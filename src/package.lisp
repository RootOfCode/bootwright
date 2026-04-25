(defpackage #:bootwright
  (:use #:cl)
  (:export
   #:*bootwright-root*
   #:*bootwright-source-type*
   #:*bootwright-machine-type*
   #:defimage
   #:defos
   #:defmachine
   #:use-machine
   #:build-image
   #:build-image-file
   #:load-machine-file
   #:find-machine
   #:load-os-source
   #:find-os-source-images
   #:build-os-source-file
   #:test-os
   #:test-os-source-file
   #:test-result
   #:test-result-image-path
   #:test-result-successp
   #:test-result-debugcon-output
   #:test-result-serial-output
   #:test-result-qemu-log
   #:test-result-exit-code
   #:test-result-duration-ms
   #:include
   #:write-image
   #:describe-image
   #:compiled-image
   #:compiled-image-bytes
   #:compiled-image-layouts
   #:compiled-image-target
   #:section-layout
   #:section-layout-name
   #:section-layout-kind
   #:section-layout-start-lba
   #:section-layout-sector-count
   #:section-layout-byte-count
   #:section-layout-load-segment
   #:section-layout-load-offset
   #:section-layout-origin
   #:section-layout-entry-offset))

(defpackage #:bootwright.os
  (:use #:cl #:bootwright))

(defpackage #:bootwright.machine
  (:use #:cl #:bootwright))

(in-package #:bootwright)
