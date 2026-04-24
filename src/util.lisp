(in-package #:bootwright)

(defparameter *bootwright-root*
  (let* ((source (or *load-truename* *compile-file-truename*))
         (src-dir (and source (make-pathname :name nil :type nil :defaults source))))
    (if src-dir
        (merge-pathnames #P"../" src-dir)
        *default-pathname-defaults*)))

(defun ensure-octet (value)
  (unless (typep value '(integer 0 255))
    (error "Value ~S is not an unsigned 8-bit integer." value))
  value)

(defun ensure-word (value)
  (unless (typep value '(integer 0 65535))
    (error "Value ~S is not an unsigned 16-bit integer." value))
  value)

(defun ensure-dword (value)
  (unless (typep value '(integer 0 4294967295))
    (error "Value ~S is not an unsigned 32-bit integer." value))
  value)

(defun align-up (value alignment)
  (unless (and (plusp alignment) (integerp alignment))
    (error "Alignment must be a positive integer, got ~S." alignment))
  (* (ceiling value alignment) alignment))

(defun ceiling-div (numerator denominator)
  (ceiling numerator denominator))

(defun make-octet-buffer ()
  (make-array 0
              :element-type '(unsigned-byte 8)
              :adjustable t
              :fill-pointer 0))

(defun append-octet (buffer value)
  (vector-push-extend (ensure-octet value) buffer)
  buffer)

(defun append-octets (buffer octets)
  (map nil (lambda (octet) (append-octet buffer octet)) octets)
  buffer)

(defun ascii-octets (string)
  (loop for ch across string
        for code = (char-code ch)
        do (when (> code 127)
             (error "Bootwright currently emits ASCII strings only, got ~S in ~S." ch string))
        collect code))

(defun write-octets-to-file (octets pathname)
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :element-type '(unsigned-byte 8))
    (write-sequence octets stream))
  pathname)

(defun read-octets-from-file (pathname)
  (with-open-file (stream pathname
                          :direction :input
                          :element-type '(unsigned-byte 8))
    (let* ((length (file-length stream))
           (buffer (make-array length :element-type '(unsigned-byte 8))))
      (read-sequence buffer stream)
      buffer)))

(defun pathname-directory-pathname (pathname)
  (make-pathname :name nil :type nil :defaults pathname))

(defun absolute-pathname-p (pathname)
  (let ((directory (pathname-directory pathname)))
    (and directory
         (eq (first directory) :absolute))))

(defun resolve-relative-pathname (designator root)
  (let ((pathname (pathname designator)))
    (if (absolute-pathname-p pathname)
        pathname
        (merge-pathnames pathname root))))

(defun copy-label-table (table)
  (let ((copy (make-hash-table :test (hash-table-test table))))
    (maphash (lambda (key value)
               (setf (gethash key copy) value))
             table)
    copy))

(defun canonical-token (designator)
  (string-upcase
   (etypecase designator
     (symbol (symbol-name designator))
     (string designator))))

(defun token-keyword (designator)
  (intern (canonical-token designator) :keyword))
