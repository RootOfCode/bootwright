(in-package #:bootwright)

(defparameter *bootwright-personality-type* "bwp")

(defstruct (personality-descriptor
            (:constructor make-personality-descriptor
                (&key name version description requires-phases compatible-isa
                      parent section-types routine-forms data-forms)))
  name
  version
  description
  requires-phases
  compatible-isa
  parent
  ;; Each is a hash-table keyed by KEYWORD form-head, value is a function
  ;; (lambda (environment form)) for routine/data forms or
  ;; (lambda (form target source-root)) for section types.
  section-types
  routine-forms
  data-forms)

(defvar *personality-registry* (make-hash-table :test 'eq)
  "Globally registered personalities, keyed by personality name (a symbol).")

(defvar *loaded-personalities* nil
  "Bound by LOAD-PERSONALITY-FILE to a fresh hash-table while loading a .bwp;
DEFPERSONALITY pushes the new descriptor here in addition to the global registry.")

(defvar *current-personality* nil
  "Bound by DEFPERSONALITY while its body is being expanded so that
DEFROUTINE-FORM / DEFDATA-FORM / DEFSECTION-TYPE can register on it.")

(defvar *pending-personalities* nil
  "Bound by LOAD-OS-SOURCE so USE-PERSONALITY can accumulate personalities
that the next DEFOS in the same file should pick up.")

(defvar *active-personalities* nil
  "Bound by BUILD-IMAGE to the list of personality descriptors active for the
image currently being parsed/compiled.")

(defun register-personality (descriptor)
  (setf (gethash (personality-descriptor-name descriptor) *personality-registry*)
        descriptor)
  (when (boundp '*loaded-personalities*)
    (setf (gethash (personality-descriptor-name descriptor) *loaded-personalities*)
          descriptor))
  descriptor)

(defun find-personality (designator &key (errorp t))
  (let ((value (cond ((personality-descriptor-p designator) designator)
                     ((symbolp designator)
                      (gethash designator *personality-registry*))
                     (t nil))))
    (when (and (null value) errorp)
      (error "Unknown Bootwright personality ~S." designator))
    value))

(defun parse-defpersonality-options (options)
  (let ((version nil)
        (description nil)
        (requires-phases '())
        (compatible-isa '())
        (parent nil))
    (dolist (entry options)
      (unless (and (consp entry) (keywordp (first entry)))
        (error "DEFPERSONALITY option entry must be (:key ...), got ~S." entry))
      (case (first entry)
        (:version
         (setf version (second entry)))
        (:description
         (setf description (second entry)))
        (:requires-phases
         (setf requires-phases (rest entry)))
        (:compatible-isa
         (setf compatible-isa (rest entry)))
        (:extends
         (setf parent (second entry)))
        (otherwise
         (error "Unknown DEFPERSONALITY option ~S." (first entry)))))
    (values version description requires-phases compatible-isa parent)))

(defun classify-personality-body-form (form)
  (and (consp form)
       (symbolp (first form))
       (let ((name (symbol-name (first form))))
         (cond ((string= name "DEFROUTINE-FORM") :routine)
               ((string= name "DEFDATA-FORM")    :data)
               ((string= name "DEFSECTION-TYPE") :section)))))

(defun execute-personality-body (descriptor body)
  (let ((*current-personality* descriptor))
    (dolist (form body)
      (let ((kind (classify-personality-body-form form)))
        (unless kind
          (error "DEFPERSONALITY body forms must be DEFROUTINE-FORM, DEFDATA-FORM or DEFSECTION-TYPE, got ~S." form))
        (eval form)))))

(defun materialize-personality (name options body)
  (multiple-value-bind (version description requires-phases compatible-isa parent-name)
      (parse-defpersonality-options options)
    (let* ((parent (and parent-name (find-personality parent-name)))
           (section-types (make-hash-table :test 'eq))
           (routine-forms (make-hash-table :test 'eq))
           (data-forms (make-hash-table :test 'eq)))
      (when parent
        (maphash (lambda (k v) (setf (gethash k section-types) v))
                 (personality-descriptor-section-types parent))
        (maphash (lambda (k v) (setf (gethash k routine-forms) v))
                 (personality-descriptor-routine-forms parent))
        (maphash (lambda (k v) (setf (gethash k data-forms) v))
                 (personality-descriptor-data-forms parent)))
      (let ((descriptor (make-personality-descriptor
                         :name name
                         :version version
                         :description description
                         :requires-phases requires-phases
                         :compatible-isa compatible-isa
                         :parent parent
                         :section-types section-types
                         :routine-forms routine-forms
                         :data-forms data-forms)))
        (execute-personality-body descriptor body)
        (register-personality descriptor)
        descriptor))))

(defmacro defpersonality (name options &body body)
  "Define a Bootwright personality.  NAME is a symbol; OPTIONS is a list of
(:KEY VALUE...) entries (:VERSION, :DESCRIPTION, :REQUIRES-PHASES,
:COMPATIBLE-ISA, :EXTENDS); BODY is a sequence of DEFROUTINE-FORM,
DEFDATA-FORM and DEFSECTION-TYPE forms."
  `(materialize-personality ',name ',options ',body))

(defun current-personality-or-error (caller)
  (or *current-personality*
      (error "~A may only appear inside a DEFPERSONALITY body." caller)))

(defun coerce-personality-key (head)
  (intern (string head) :keyword))

(defmacro defroutine-form (head lambda-list &body body)
  "Inside DEFPERSONALITY: declare a routine-level form named HEAD.  When this
form appears in an OS source, the personality system invokes BODY with
ENVIRONMENT and FORM lexically bound (FORM is the full source list), and
the form tail is DESTRUCTURING-BINDed against LAMBDA-LIST."
  (let ((head-key (coerce-personality-key head))
        (descriptor (gensym "DESC-"))
        (environment-name (intern "ENVIRONMENT" *package*))
        (form-name (intern "FORM" *package*))
        (environment-var (gensym "ENV-"))
        (form-var (gensym "FORM-")))
    `(let ((,descriptor (current-personality-or-error 'defroutine-form)))
       (setf (gethash ,head-key (personality-descriptor-routine-forms ,descriptor))
             (lambda (,environment-var ,form-var)
               (let ((,environment-name ,environment-var)
                     (,form-name ,form-var))
                 (declare (ignorable ,environment-name ,form-name))
                 (destructuring-bind ,lambda-list (rest ,form-name)
                   ,@body)))))))

(defmacro defdata-form (head lambda-list &body body)
  "Inside DEFPERSONALITY: declare a data-level form named HEAD.  BODY runs
with ENVIRONMENT and FORM lexically bound, and the form tail is
DESTRUCTURING-BINDed against LAMBDA-LIST."
  (let ((head-key (coerce-personality-key head))
        (descriptor (gensym "DESC-"))
        (environment-name (intern "ENVIRONMENT" *package*))
        (form-name (intern "FORM" *package*))
        (environment-var (gensym "ENV-"))
        (form-var (gensym "FORM-")))
    `(let ((,descriptor (current-personality-or-error 'defdata-form)))
       (setf (gethash ,head-key (personality-descriptor-data-forms ,descriptor))
             (lambda (,environment-var ,form-var)
               (let ((,environment-name ,environment-var)
                     (,form-name ,form-var))
                 (declare (ignorable ,environment-name ,form-name))
                 (destructuring-bind ,lambda-list (rest ,form-name)
                   ,@body)))))))

(defmacro defsection-type (head lambda-list &body body)
  "Inside DEFPERSONALITY: declare a top-level section-type named HEAD.  BODY
runs with FORM, TARGET and SOURCE-ROOT lexically bound and must return a
SECTION-SPEC.  The form tail is DESTRUCTURING-BINDed against LAMBDA-LIST."
  (let ((head-key (coerce-personality-key head))
        (descriptor (gensym "DESC-"))
        (form-name (intern "FORM" *package*))
        (target-name (intern "TARGET" *package*))
        (source-root-name (intern "SOURCE-ROOT" *package*))
        (form-var (gensym "FORM-"))
        (target-var (gensym "TARGET-"))
        (source-root-var (gensym "SOURCE-ROOT-")))
    `(let ((,descriptor (current-personality-or-error 'defsection-type)))
       (setf (gethash ,head-key (personality-descriptor-section-types ,descriptor))
             (lambda (,form-var ,target-var ,source-root-var)
               (let ((,form-name ,form-var)
                     (,target-name ,target-var)
                     (,source-root-name ,source-root-var))
                 (declare (ignorable ,form-name ,target-name ,source-root-name))
                 (destructuring-bind ,lambda-list (rest ,form-name)
                   ,@body)))))))

(defun normalize-personality-pathname (designator base-pathname)
  (typecase designator
    (pathname
     (let* ((target (if base-pathname
                        (merge-pathnames designator base-pathname)
                        designator)))
       (if (pathname-type target)
           target
           (make-pathname :type *bootwright-personality-type*
                          :defaults target))))
    (string
     (normalize-personality-pathname (pathname designator) base-pathname))
    (t
     (error "Cannot resolve personality pathname designator ~S." designator))))

(defun load-personality-file (pathname)
  "Load a .bwp file in the BOOTWRIGHT.PERSONALITY package and return the
descriptor it defined.  Errors if the file does not produce exactly one
personality."
  (let* ((target-package (or (find-package '#:bootwright.personality)
                             (error "Package BOOTWRIGHT.PERSONALITY is missing.")))
         (*package* target-package)
         (*loaded-personalities* (make-hash-table :test 'eq)))
    (load pathname :verbose nil :print nil)
    (let ((personalities (loop for v being the hash-values of *loaded-personalities*
                               collect v)))
      (unless personalities
        (error "Personality file ~A defined no DEFPERSONALITY." pathname))
      (unless (= (length personalities) 1)
        (error "Personality file ~A defined ~D personalities; exactly one is required."
               pathname
               (length personalities)))
      (first personalities))))

(defun build-personality-file (pathname)
  (load-personality-file pathname))

(defun resolve-personality-designator (designator)
  (cond ((personality-descriptor-p designator)
         designator)
        ((symbolp designator)
         (find-personality designator :errorp t))
        ((or (pathnamep designator) (stringp designator))
         (let* ((base (or *load-truename* *load-pathname*))
                (resolved (normalize-personality-pathname designator base)))
           (load-personality-file resolved)))
        (t
         (error "Cannot resolve personality designator ~S." designator))))

(defun use-personality (designator)
  "Activate a personality for the next DEFOS in this load context.  When a
DESIGNATOR is a pathname or string, the .bwp file is loaded; when it is a
symbol, the personality must already be registered."
  (let ((descriptor (resolve-personality-designator designator)))
    (when (boundp '*pending-personalities*)
      (pushnew (personality-descriptor-name descriptor)
               *pending-personalities*
               :test #'eq))
    descriptor))

(defun take-pending-personalities ()
  (when (boundp '*pending-personalities*)
    (let ((names (nreverse *pending-personalities*)))
      (setf *pending-personalities* nil)
      names)))

(defun resolve-active-personalities (names)
  (mapcar (lambda (name) (find-personality name :errorp t)) names))

(defun lookup-active-personality-section-type (head)
  (let ((key (and (symbolp head) (coerce-personality-key head))))
    (when key
      (dolist (descriptor *active-personalities*)
        (let ((entry (gethash key (personality-descriptor-section-types descriptor))))
          (when entry
            (return entry)))))))

(defun lookup-active-personality-form (head)
  (let ((key (and (symbolp head) (coerce-personality-key head))))
    (when key
      (dolist (descriptor *active-personalities*)
        (let ((entry (or (gethash key (personality-descriptor-routine-forms descriptor))
                         (gethash key (personality-descriptor-data-forms descriptor)))))
          (when entry
            (return entry)))))))
