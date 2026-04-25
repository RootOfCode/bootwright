(in-package #:bootwright)

(defparameter *bootwright-machine-type* "bwm")

(defvar *loaded-machines* nil)
(defvar *machine-registry* (make-hash-table :test 'equal))

(defun normalize-machine-pathname (designator &optional (defaults (or *default-pathname-defaults*
                                                                      *bootwright-root*)))
  (let ((pathname (pathname designator)))
    (merge-pathnames
     (if (pathname-type pathname)
         pathname
         (make-pathname :type *bootwright-machine-type* :defaults pathname))
     defaults)))

(defun machine-registry-key (designator)
  (etypecase designator
    (machine-descriptor
     (normalize-device-name (machine-descriptor-name designator)))
    (symbol
     (normalize-device-name designator))
    (string
     (normalize-device-name designator))))

(defun register-machine-definition (machine)
  (let ((key (machine-registry-key machine)))
    (setf (gethash key *machine-registry*) machine)
    (when (boundp '*loaded-machines*)
      (pushnew key *loaded-machines* :test #'equal))
    machine))

(defun parse-memory-region-form (form)
  (destructuring-bind (kind &rest args) form
    (unless (eq (token-keyword kind) :region)
      (error "Unsupported memory region form ~S." kind))
    (make-memory-region :name (getf args :name)
                        :start (getf args :start)
                        :end (getf args :end)
                        :type (getf args :type))))

(defun parse-boot-protocol-form (name args)
  (ecase (token-keyword name)
    (:bios-mbr
     (make-bios-mbr-protocol :load-address (getf args :load-address #x7C00)
                             :sector-size (getf args :sector-size 512)
                             :kernel-segment (getf args :kernel-segment #x1000)
                             :signature (getf args :signature #xAA55)))))

(defun parse-interrupt-controller-form (form)
  (destructuring-bind (kind &rest args) form
    (ecase (token-keyword kind)
      (:pic8259
       (make-pic8259-controller :master-base (getf args :master-base #x20)
                                :slave-base (getf args :slave-base #xA0)
                                :master-mask-port (getf args :master-mask-port #x21)
                                :slave-mask-port (getf args :slave-mask-port #xA1)
                                :vector-base (getf args :vector-base 32)
                                :slave-identity (getf args :slave-identity 2))))))

(defun parse-timer-form (form)
  (destructuring-bind (kind &rest args) form
    (ecase (token-keyword kind)
      (:pit8253
       (make-pit8253-timer :channel0-port (getf args :channel0-port #x40)
                           :channel1-port (getf args :channel1-port #x41)
                           :channel2-port (getf args :channel2-port #x42)
                           :control-port (getf args :control-port #x43)
                           :irq (getf args :irq 0)
                           :base-frequency (getf args :base-frequency 1193182))))))

(defun parse-serial-form (form)
  (destructuring-bind (kind &rest args) form
    (ecase (token-keyword kind)
      (:uart16550
       (make-uart16550-device :name (normalize-device-name (or (getf args :name)
                                                               (error "UART16550 requires :NAME.")))
                              :base-port (getf args :base-port #x3F8)
                              :irq (getf args :irq 4)
                              :baud (getf args :baud 115200)
                              :divisor (getf args :divisor 1))))))

(defun parse-framebuffer-form (form)
  (destructuring-bind (kind &rest args) form
    (ecase (token-keyword kind)
      (:vga-text
       (make-vga-text-framebuffer :name (normalize-device-name (or (getf args :name)
                                                                   (error "VGA-TEXT framebuffer requires :NAME.")))
                                  :columns (getf args :columns 80)
                                  :rows (getf args :rows 25)
                                  :base-address (getf args :base-address
                                                       (getf args :base #xB8000))
                                  :default-attribute (getf args :default-attribute #x07))))))

(defun parse-machine-definition (name clauses)
  (let ((isa nil)
        (boot-protocol nil)
        (memory-regions '())
        (interrupt-controller nil)
        (timer-device nil)
        (serial-devices '())
        (framebuffer-devices '()))
    (dolist (clause clauses)
      (let ((kind (token-keyword (first clause))))
        (case kind
          (:isa
           (setf isa (second clause)))
          (:boot-protocol
           (setf boot-protocol (parse-boot-protocol-form (second clause) (cddr clause))))
          (:memory
           (setf memory-regions (mapcar #'parse-memory-region-form (rest clause))))
          (:interrupt-controller
           (setf interrupt-controller (parse-interrupt-controller-form (second clause))))
          (:timer
           (setf timer-device (parse-timer-form (second clause))))
          (:serial
           (setf serial-devices (mapcar #'parse-serial-form (rest clause))))
          (:framebuffer
           (setf framebuffer-devices (mapcar #'parse-framebuffer-form (rest clause))))
          (t
           (error "Unsupported machine clause ~S." (first clause))))))
    (unless boot-protocol
      (error "Machine ~S requires a :BOOT-PROTOCOL clause." name))
    (make-machine-descriptor :name (normalize-device-name name)
                             :isa isa
                             :boot-protocol boot-protocol
                             :memory-regions memory-regions
                             :interrupt-controller interrupt-controller
                             :timer-device timer-device
                             :serial-devices serial-devices
                             :framebuffer-devices framebuffer-devices)))

(defmacro defmachine (name &body clauses)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (register-machine-definition
      (parse-machine-definition ',name ',clauses))))

(defun find-machine (designator)
  (gethash (machine-registry-key designator) *machine-registry*))

(defun load-machine-file (pathname)
  (let* ((source (normalize-machine-pathname pathname))
         (*package* (or (find-package '#:bootwright.machine)
                        (error "Bootwright machine package is not available.")))
         (*loaded-machines* '()))
    (load source :verbose nil :print nil)
    (let ((machines (mapcar #'find-machine (nreverse *loaded-machines*))))
      (unless machines
        (error "Bootwright machine source ~A did not define any DEFMACHINE forms." source))
      machines)))

(defun use-machine (designator)
  (cond ((typep designator '(or pathname string))
         (let* ((base (or *load-truename* *load-pathname* *bootwright-root*))
                (root (pathname-directory-pathname base))
                (resolved (normalize-machine-pathname designator root)))
           (load-machine-file resolved)))
        (t
         (or (find-machine designator)
             (error "Unknown machine ~S." designator)))))
