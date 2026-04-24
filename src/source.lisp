(in-package #:bootwright)

(defparameter *bootwright-source-type* "bwo")

(defvar *loaded-os-images* nil)
(defvar *os-source-registry* (make-hash-table :test 'equal))

(defun normalize-os-source-pathname (designator)
  (let* ((pathname (pathname designator))
         (defaults (or *default-pathname-defaults* *bootwright-root*)))
    (merge-pathnames
     (if (pathname-type pathname)
         pathname
         (make-pathname :type *bootwright-source-type* :defaults pathname))
     defaults)))

(defun os-source-key (pathname)
  (namestring (merge-pathnames pathname)))

(defun register-os-definition (name &optional (pathname (or *load-truename* *load-pathname*)))
  (when pathname
    (let* ((key (os-source-key pathname))
           (existing (gethash key *os-source-registry*)))
      (setf (gethash key *os-source-registry*)
            (adjoin name existing :test #'eq))))
  (when (boundp '*loaded-os-images*)
    (pushnew name *loaded-os-images* :test #'eq))
  name)

(defmacro defos (name options &body sections)
  `(progn
     (defimage ,name ,options ,@sections)
     (eval-when (:load-toplevel :execute)
       (register-os-definition ',name))
     ',name))

(defun find-os-source-images (pathname)
  (copy-list (gethash (os-source-key (normalize-os-source-pathname pathname))
                      *os-source-registry*)))

(defun load-os-source (pathname &key (package '#:bootwright.os))
  (let* ((source (normalize-os-source-pathname pathname))
         (target-package (or (find-package package)
                             (error "Unknown package ~S for Bootwright source loading." package)))
         (*package* target-package)
         (*loaded-os-images* '()))
    (load source :verbose nil :print nil)
    (let ((images (nreverse *loaded-os-images*)))
      (unless images
        (error "Bootwright source ~A did not define any DEFOS images." source))
      images)))

(defun default-os-image-output (pathname)
  (merge-pathnames (make-pathname :name (pathname-name pathname)
                                  :type "img")
                   (merge-pathnames #P"out/" *bootwright-root*)))

(defun select-os-image (pathname image images)
  (cond (image
         image)
        ((= (length images) 1)
         (first images))
        (t
         (error "Bootwright source ~A defines multiple OS images (~{~S~^, ~}); specify :IMAGE."
                pathname
                images))))

(defun build-os-source-file (pathname &key output image)
  (let* ((source (normalize-os-source-pathname pathname))
         (images (load-os-source source))
         (designator (select-os-image source image images)))
    (build-image-file designator (or output (default-os-image-output source)))))
