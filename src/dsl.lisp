(in-package #:bootwright)

(defstruct (compile-environment
            (:constructor make-compile-environment
                (&key target section layouts state helpers constants keyboards
                      block-devices memory-maps phys-allocators page-structures syscall-tables
                      slot-types partition-tables volumes)))
  target
  section
  layouts
  state
  helpers
  constants
  keyboards
  block-devices
  memory-maps
  phys-allocators
  page-structures
  syscall-tables
  slot-types
  partition-tables
  volumes)

(defun helper-needed-p (environment helper)
  (gethash helper (compile-environment-helpers environment)))

(defun mark-helper-needed (environment helper)
  (let* ((state (compile-environment-state environment))
         (bits (assembly-bits state))
         (existing (gethash helper (compile-environment-helpers environment))))
    (when (and existing (/= existing bits))
      (error "Helper ~S is requested in both ~D-bit and ~D-bit code in the same section."
             helper existing bits))
    (setf (gethash helper (compile-environment-helpers environment)) bits)))

(defun require-bits (environment bits form-name)
  (unless (= (assembly-bits (compile-environment-state environment)) bits)
    (error "~A requires ~D-bit code generation in the current section."
           form-name bits)))

(defun constant-defined-p (environment name)
  (multiple-value-bind (_ present)
      (gethash name (compile-environment-constants environment))
    (declare (ignore _))
    present))

(defun resolve-operand (environment operand)
  (cond ((and (symbolp operand)
              (constant-defined-p environment operand))
         (resolve-operand environment
                          (gethash operand (compile-environment-constants environment))))
        ((consp operand)
         (mapcar (lambda (item) (resolve-operand environment item)) operand))
        (t
         operand)))

(defun resolve-operands (environment operands)
  (mapcar (lambda (operand) (resolve-operand environment operand)) operands))

(defun section-source-root (environment)
  (or (section-spec-source-root (compile-environment-section environment))
      *bootwright-root*))

(defun compile-nested-forms (environment forms)
  (dolist (nested-form forms)
    (compile-form environment nested-form)))

(defun parse-conditional-clauses (form operator-name)
  (destructuring-bind (_ operand &rest clauses) form
    (declare (ignore _))
    (let ((then-body nil)
          (else-body nil))
      (dolist (clause clauses)
        (unless (consp clause)
          (error "~A clauses must be lists, got ~S." operator-name clause))
        (case (token-keyword (first clause))
          (:then
           (when then-body
             (error "~A accepts at most one :THEN clause." operator-name))
           (setf then-body (rest clause)))
          (:else
           (when else-body
             (error "~A accepts at most one :ELSE clause." operator-name))
           (setf else-body (rest clause)))
          (t
           (error "~A clause must start with :THEN or :ELSE, got ~S."
                  operator-name
                  clause))))
      (unless then-body
        (error "~A requires a :THEN clause." operator-name))
      (values operand then-body else-body))))

(defun emit-conditional-zero-form (environment form then-on-zero-p)
  (let* ((state (compile-environment-state environment))
         (operator-name (canonical-token (first form))))
    (multiple-value-bind (operand then-body else-body)
        (parse-conditional-clauses form operator-name)
      (let* ((resolved (resolve-operand environment operand))
             (else-label (gensym "IF-ELSE-"))
             (done-label (and else-body (gensym "IF-DONE-"))))
        (encode-instruction state 'cmp (list resolved 0))
        (encode-instruction state
                            (if then-on-zero-p 'jne-near 'je-near)
                            (list else-label))
        (compile-nested-forms environment then-body)
        (when else-body
          (encode-instruction state 'jmp (list done-label)))
        (emit-label state else-label)
        (when else-body
          (compile-nested-forms environment else-body)
          (emit-label state done-label))))))

(defun emit-while-zero-form (environment form continue-on-zero-p)
  (let* ((state (compile-environment-state environment))
         (operator-name (canonical-token (first form))))
    (destructuring-bind (_ operand &body body) form
      (declare (ignore _))
      (unless body
        (error "~A requires at least one body form." operator-name))
      (let ((start-label (gensym "WHILE-START-"))
            (done-label (gensym "WHILE-DONE-"))
            (resolved (resolve-operand environment operand)))
        (emit-label state start-label)
        (encode-instruction state 'cmp (list resolved 0))
        (encode-instruction state
                            (if continue-on-zero-p 'jne-near 'je-near)
                            (list done-label))
        (compile-nested-forms environment body)
        (encode-instruction state 'jmp (list start-label))
        (emit-label state done-label)))))

(defun emit-forever-form (environment form)
  (let* ((state (compile-environment-state environment))
         (operator-name (canonical-token (first form))))
    (destructuring-bind (_ &body body) form
      (declare (ignore _))
      (unless body
        (error "~A requires at least one body form." operator-name))
      (let ((start-label (gensym "FOREVER-START-")))
        (emit-label state start-label)
        (compile-nested-forms environment body)
        (encode-instruction state 'jmp (list start-label))))))

(defun environment-machine (environment)
  (compile-environment-target environment))

(defun machine-interrupt-controller-or-die (environment)
  (or (machine-descriptor-interrupt-controller (environment-machine environment))
      (error "The current machine does not define an interrupt controller.")))

(defun machine-timer-device-or-die (environment)
  (or (machine-descriptor-timer-device (environment-machine environment))
      (error "The current machine does not define a timer device.")))

(defun default-framebuffer-device (machine)
  (or (first (machine-descriptor-framebuffer-devices machine))
      (error "The current machine does not define any framebuffer devices.")))

(defun resolve-framebuffer-device (environment designator)
  (let ((machine (environment-machine environment)))
    (if designator
        (find-machine-framebuffer machine designator)
        (default-framebuffer-device machine))))

(defun default-serial-device (machine)
  (or (first (machine-descriptor-serial-devices machine))
      (error "The current machine does not define any serial devices.")))

(defun resolve-serial-device (environment designator)
  (let ((machine (environment-machine environment)))
    (if designator
        (find-machine-serial-device machine designator)
        (default-serial-device machine))))

(defun resolve-section-pathname (environment designator)
  (resolve-relative-pathname designator (section-source-root environment)))

(defun ascii-display-char (designator)
  (let ((character (cond ((characterp designator)
                          designator)
                         ((and (stringp designator) (= (length designator) 1))
                          (char designator 0))
                         ((and (integerp designator)
                               (typep designator '(integer 0 127)))
                          (code-char designator))
                         (t
                          (error "Expected an ASCII character designator, got ~S." designator)))))
    (when (> (char-code character) 127)
      (error "Character ~S is outside the ASCII range." character))
    character))

(defun make-filled-string (length designator)
  (make-string length :initial-element (ascii-display-char designator)))

(defun resolve-vga-address (args)
  (or (getf args :address)
      (let* ((row (getf args :row 0))
             (column (getf args :column 0))
             (columns (getf args :columns 80))
             (base-address (getf args :base-address #xB8000)))
        (+ base-address (* 2 (+ column (* row columns)))))))

(defun normalize-data-item (item)
  (normalize-immediate item))

(defun emit-db (state items)
  (dolist (item items)
    (cond ((integerp item)
           (emit-u8 state item))
          ((stringp item)
           (append-octets (assembly-state-bytes state) (ascii-octets item)))
          (t
           (error "DB accepts integers and strings, got ~S." item)))))

(defun emit-dw (state items)
  (dolist (item items)
    (let ((normalized (normalize-data-item item)))
      (cond ((integerp normalized)
             (emit-u16 state normalized))
            ((or (symbolp normalized)
                 (label-reference-p normalized)
                 (linear-reference-p normalized)
                 (low16-reference-p normalized)
                 (high16-reference-p normalized))
             (emit-imm16 state normalized))
            (t
             (error "DW accepts integers and labels, got ~S." item))))))

(defun emit-dd (state items)
  (dolist (item items)
    (let ((normalized (normalize-data-item item)))
      (cond ((integerp normalized)
             (emit-u32 state normalized))
            ((or (symbolp normalized)
                 (label-reference-p normalized)
                 (linear-reference-p normalized)
                 (linear-addend-reference-p normalized))
             (emit-imm32 state normalized))
            (t
             (error "DD accepts integers and labels, got ~S." item))))))

(defun emit-include-binary (environment form)
  (let* ((state (compile-environment-state environment))
         (name (second form))
         (pathname (resolve-operand environment (third form)))
         (args (resolve-operands environment (cdddr form)))
         (alignment (getf args :align))
         (zero-terminated (getf args :zero-terminated nil))
         (bytes (read-octets-from-file (resolve-section-pathname environment pathname))))
    (when alignment
      (emit-align state alignment))
    (emit-label state name)
    (append-octets (assembly-state-bytes state) bytes)
    (when zero-terminated
      (emit-u8 state 0))))

(defun emit-ascii-z (state string)
  (append-octets (assembly-state-bytes state) (ascii-octets string))
  (emit-u8 state 0))

(defun emit-align (state alignment &optional (fill 0))
  (let* ((current (assembly-address state))
         (next (align-up current alignment))
         (padding (- next current)))
    (loop repeat padding do
      (emit-u8 state fill))))

(defun emit-pad-to (state target-offset &optional (fill 0))
  "Emit FILL bytes until the section's current offset reaches TARGET-OFFSET.
Errors if the current offset is already past TARGET-OFFSET."
  (let* ((current (assembly-offset state))
         (padding (- target-offset current)))
    (when (minusp padding)
      (error "PAD-TO ~D underflows: section already at offset ~D." target-offset current))
    (loop repeat padding do
      (emit-u8 state fill))))

(defun descriptor-base (environment value)
  (cond ((or (null value) (eql value 0))
         0)
        ((eq value :section)
         (section-linear-base (compile-environment-section environment)))
        ((integerp value)
         (ensure-dword value))
        (t
         (error "Unsupported descriptor base ~S." value))))

(defun emit-segment-descriptor (state base limit access flags)
  (unless (typep limit '(integer 0 #xFFFFF))
    (error "Descriptor limit must fit in 20 bits, got ~S." limit))
  (emit-u16 state (ldb (byte 16 0) limit))
  (emit-u16 state (ldb (byte 16 0) base))
  (emit-u8 state (ldb (byte 8 16) base))
  (emit-u8 state access)
  (emit-u8 state (logior (ldb (byte 4 16) limit)
                         (ash flags 4)))
  (emit-u8 state (ldb (byte 8 24) base)))

(defun emit-segment-descriptor-with-label-base (state label limit access flags)
  "Emit an 8-byte segment/system descriptor whose base field references a label
by linear address. Three fixups patch base[15:0], base[23:16], and base[31:24]."
  (unless (typep limit '(integer 0 #xFFFFF))
    (error "Descriptor limit must fit in 20 bits, got ~S." limit))
  (emit-u16 state (ldb (byte 16 0) limit))
  (let ((low16-pos (assembly-offset state)))
    (emit-u16 state 0)
    (add-fixup state :linear-low16 label low16-pos))
  (let ((mid8-pos (assembly-offset state)))
    (emit-u8 state 0)
    (add-fixup state :linear-mid8 label mid8-pos))
  (emit-u8 state access)
  (emit-u8 state (logior (ldb (byte 4 16) limit)
                         (ash flags 4)))
  (let ((high8-pos (assembly-offset state)))
    (emit-u8 state 0)
    (add-fixup state :linear-high8 label high8-pos)))

(defun make-segment-access-byte (kind options)
  (let* ((dpl (getf options :dpl 0))
         (present (if (getf options :present t) #x80 0))
         (descriptor-type #x10)
         (executable (if (eq kind :code) #x08 0))
         (direction/conforming (if (getf options (if (eq kind :code) :conforming :expand-down) nil)
                                   #x04
                                   0))
         (read/write (if (getf options (if (eq kind :code) :readable :writable) t)
                         #x02
                         0))
         (accessed (if (getf options :accessed nil) #x01 0)))
    (logior present
            descriptor-type
            (ash dpl 5)
            executable
            direction/conforming
            read/write
            accessed)))

(defun make-segment-flags-nibble (options)
  (logior (if (getf options :granularity nil) #x8 0)
          (if (getf options :db nil) #x4 0)
          (if (getf options :long-mode nil) #x2 0)
          (if (getf options :available nil) #x1 0)))

(defun parse-leading-options (rest allowed-keys)
  (if (and rest
           (consp (first rest))
           (let ((first-key (ignore-errors (token-keyword (first (first rest))))))
             (and first-key
                  (member first-key allowed-keys))))
      (values (first rest) (rest rest))
      (values '() rest)))

(defun make-idt-type-attr (options)
  (let* ((present (if (getf options :present t) #x80 0))
         (dpl (ash (getf options :dpl 0) 5))
         (gate-type (getf options :gate-type :interrupt32))
         (type-bits (ecase gate-type
                      (:interrupt32 #x0E)
                      (:trap32 #x0F))))
    (logior present dpl type-bits)))

(defun emit-gdt-entry (environment descriptor)
  (let* ((state (compile-environment-state environment))
         (kind (token-keyword (first descriptor)))
         (options (resolve-operands environment (rest descriptor))))
    (case kind
      (:null
       (emit-dd state '(0 0)))
      (:code32
       (emit-segment-descriptor
        state
        (descriptor-base environment (getf options :base 0))
        (getf options :limit #xFFFFF)
        (make-segment-access-byte :code options)
        (make-segment-flags-nibble (list* :db t options))))
      (:data32
       (emit-segment-descriptor
        state
        (descriptor-base environment (getf options :base 0))
        (getf options :limit #xFFFFF)
        (make-segment-access-byte :data options)
        (make-segment-flags-nibble (list* :db t options))))
      (:code16
       (emit-segment-descriptor
        state
        (descriptor-base environment (getf options :base 0))
        (getf options :limit #xFFFF)
        (make-segment-access-byte :code options)
        (make-segment-flags-nibble options)))
      (:data16
       (emit-segment-descriptor
        state
        (descriptor-base environment (getf options :base 0))
        (getf options :limit #xFFFF)
        (make-segment-access-byte :data options)
        (make-segment-flags-nibble options)))
      (:tss
       (let* ((label (or (getf options :label)
                         (error ":TSS GDT entry requires :LABEL.")))
              (dpl (getf options :dpl 0))
              (access (logior #x80 (ash dpl 5) #x09))
              (limit (getf options :limit 103)))
         (unless (symbolp label)
           (error ":TSS GDT entry expects a label symbol, got ~S." label))
         (emit-segment-descriptor-with-label-base state label limit access 0)))
      (t
       (error "Unsupported GDT entry kind ~S." (first descriptor))))))

(defun emit-gdt (environment form)
  (let* ((state (compile-environment-state environment))
         (name (second form))
         (rest (cddr form)))
    (multiple-value-bind (options descriptors)
        (parse-leading-options rest '(:pointer))
      (let ((options (resolve-operands environment options))
            (pointer-label nil))
        (setf pointer-label (getf options :pointer))
        (emit-label state name)
        (dolist (descriptor descriptors)
          (emit-gdt-entry environment descriptor))
        (when pointer-label
          (emit-label state pointer-label)
          (emit-dw state (list (1- (* 8 (length descriptors)))))
          (emit-dd state (list (list :linear name))))))))

(defun emit-idt-gate (state selector handler type-attr)
  (emit-dw state (list (list :low16 handler)))
  (emit-dw state (list selector))
  (emit-db state (list 0 type-attr))
  (emit-dw state (list (list :high16 handler))))

(defun emit-idt (environment form)
  (let* ((state (compile-environment-state environment))
         (name (second form))
         (rest (cddr form)))
    (require-bits environment 32 "IDT")
    (multiple-value-bind (options entries)
        (parse-leading-options rest '(:pointer :selector :default :count :type-attr :gate-type :dpl :present))
      (let* ((options (resolve-operands environment options))
             (count (getf options :count 256))
             (default-handler (getf options :default))
             (default-selector (getf options :selector #x08))
             (default-type-attr (or (getf options :type-attr)
                                    (make-idt-type-attr options)))
             (pointer-label (getf options :pointer))
             (vector-table (make-array count :initial-element nil)))
        (unless default-handler
          (error "IDT ~S requires a :DEFAULT handler." name))
        (dotimes (index count)
          (setf (aref vector-table index)
                (list default-handler default-selector default-type-attr)))
        (dolist (entry entries)
          (destructuring-bind (kind vector handler &rest entry-options)
              (resolve-operands environment entry)
            (unless (eq (token-keyword kind) :entry)
              (error "Unsupported IDT entry form ~S." kind))
            (unless (typep vector `(integer 0 ,(1- count)))
              (error "IDT vector ~S is out of range 0..~D." vector (1- count)))
            (setf (aref vector-table vector)
                  (list handler
                        (getf entry-options :selector default-selector)
                        (or (getf entry-options :type-attr)
                            (make-idt-type-attr (append entry-options
                                                        (list :present t
                                                              :gate-type (or (getf entry-options :gate-type)
                                                                             :interrupt32)))))))))
        (emit-label state name)
        (dotimes (vector count)
          (destructuring-bind (handler selector type-attr)
              (aref vector-table vector)
            (emit-idt-gate state selector handler type-attr)))
        (when pointer-label
          (emit-label state pointer-label)
          (emit-dw state (list (1- (* count 8))))
          (emit-dd state (list (list :linear name))))))))

(defstruct (page-structures-descriptor
            (:constructor make-page-structures-descriptor
                (&key name levels granularity base-address
                      coverage-start coverage-end flags)))
  name
  levels
  granularity
  base-address
  coverage-start
  coverage-end
  flags)

(defun unwrap-quoted-value (value)
  (if (and (consp value)
           (eq (first value) 'quote)
           (= (length value) 2))
      (second value)
      value))

(defun parse-page-structures-options (environment raw-options)
  (let ((items (resolve-operands environment raw-options))
        (levels 2)
        (granularity :4kb)
        (base-address nil)
        (coverage-start nil)
        (coverage-end nil)
        (flags '(:present :writable)))
    (loop while items do
      (let ((key (token-keyword (pop items))))
        (case key
          (:levels
           (setf levels (or (pop items)
                            (error "PAGE-STRUCTURES :LEVELS requires a value."))))
          (:granularity
           (setf granularity (or (pop items)
                                 (error "PAGE-STRUCTURES :GRANULARITY requires a value."))))
          (:base-address
           (setf base-address (or (pop items)
                                  (error "PAGE-STRUCTURES :BASE-ADDRESS requires a value."))))
          (:coverage
           (if (and items (consp (first items)))
               (destructuring-bind (start end) (pop items)
                 (setf coverage-start start
                       coverage-end end))
               (let ((start (pop items))
                     (end (pop items)))
                 (when (or (null start) (null end))
                   (error "PAGE-STRUCTURES :COVERAGE requires a start and end address."))
                 (setf coverage-start start
                       coverage-end end))))
          (:flags
           (setf flags (or (pop items)
                           (error "PAGE-STRUCTURES :FLAGS requires a value."))))
          (t
           (error "Unsupported PAGE-STRUCTURES option ~S." key)))))
    (values levels granularity base-address coverage-start coverage-end flags)))

(defun parse-page-structures-form (environment form)
  (let ((name (or (second form)
                  (error "PAGE-STRUCTURES requires a name."))))
    (multiple-value-bind (levels granularity base-address coverage-start coverage-end flags)
        (parse-page-structures-options environment
                                       (or (third form)
                                           (error "PAGE-STRUCTURES ~S requires an options list." name)))
      (unless (and (integerp levels) (<= 1 levels 4))
        (error "PAGE-STRUCTURES :LEVELS must be an integer from 1 to 4, got ~S." levels))
      (setf granularity (token-keyword granularity))
      (unless (member granularity '(:4kb :4mib))
        (error "PAGE-STRUCTURES currently supports :4KB and :4MIB granularities, got ~S."
               granularity))
      (unless (and (integerp coverage-start)
                   (integerp coverage-end)
                   (<= 0 coverage-start coverage-end #xFFFFFFFF))
        (error "PAGE-STRUCTURES :COVERAGE must be two 32-bit addresses in ascending order, got ~S .. ~S."
               coverage-start coverage-end))
      (when base-address
        (unless (and (integerp base-address) (<= 0 base-address #xFFFFFFFF))
          (error "PAGE-STRUCTURES :BASE-ADDRESS must be a 32-bit non-negative integer, got ~S."
                 base-address)))
      (make-page-structures-descriptor
       :name name
       :levels levels
       :granularity granularity
       :base-address base-address
       :coverage-start coverage-start
       :coverage-end coverage-end
       :flags flags))))

(defun register-page-structures (environment form)
  (let* ((descriptor (parse-page-structures-form environment form))
         (name (page-structures-descriptor-name descriptor)))
    (when (gethash name (compile-environment-page-structures environment))
      (error "PAGE-STRUCTURES ~S is already declared in this section." name))
    (setf (gethash name (compile-environment-page-structures environment)) descriptor)
    descriptor))

(defun lookup-page-structures (environment name)
  (or (gethash name (compile-environment-page-structures environment))
      (error "Unknown page-structures ~S — is (page-structures ~S ...) declared in this section?"
             name name)))

(defun page-entry-unit-size (descriptor)
  (ecase (page-structures-descriptor-granularity descriptor)
    (:4kb 4096)
    (:4mib #x400000)))

(defun page-directory-index-range (descriptor)
  (values (truncate (page-structures-descriptor-coverage-start descriptor) #x400000)
          (truncate (page-structures-descriptor-coverage-end descriptor) #x400000)))

(defun page-coverage-contains-p (descriptor address)
  (and (integerp address)
       (<= (page-structures-descriptor-coverage-start descriptor)
           address
           (page-structures-descriptor-coverage-end descriptor))))

(defun page-structure-table-label (descriptor directory-index)
  (intern (format nil "~A-TABLE-~D"
                  (symbol-name (page-structures-descriptor-name descriptor))
                  directory-index)
          (or (symbol-package (page-structures-descriptor-name descriptor))
              *package*)))

(defun page-flags-mask (flags &key table-pointer large-page)
  (let* ((flags (unwrap-quoted-value flags))
         (value (cond ((integerp flags)
                       flags)
                      ((null flags)
                       0)
                      ((listp flags)
                       (loop for flag in flags
                             for keyword = (token-keyword flag)
                             sum (case keyword
                                   (:present #x001)
                                   (:writable #x002)
                                   (:writeable #x002)
                                   (:user #x004)
                                   (:write-through #x008)
                                   (:cache-disable #x010)
                                   (:accessed #x020)
                                   (:dirty #x040)
                                   (:page-size #x080)
                                   (:large-page #x080)
                                   (:global #x100)
                                   (t
                                    (error "Unknown paging flag ~S." flag)))))
                      (t
                       (error "Unsupported paging flags designator ~S." flags)))))
    (when large-page
      (setf value (logior value #x080)))
    (if table-pointer
        (logand value #x03F)
        value)))

(defun emit-page-structures (environment form)
  (let* ((state (compile-environment-state environment))
         (descriptor (lookup-page-structures environment (second form)))
         (name (page-structures-descriptor-name descriptor))
         (levels (page-structures-descriptor-levels descriptor))
         (granularity (page-structures-descriptor-granularity descriptor))
         (base-address (page-structures-descriptor-base-address descriptor))
         (coverage-start (page-structures-descriptor-coverage-start descriptor))
         (coverage-end (page-structures-descriptor-coverage-end descriptor))
         (flags (page-structures-descriptor-flags descriptor))
         (section-base (section-linear-base (compile-environment-section environment))))
    (unless (= levels 2)
      (error "PAGE-STRUCTURES currently supports only 2-level paging on x86-32, got :LEVELS ~S."
             levels))
    (if base-address
        (progn
          (unless (zerop (mod base-address 4096))
            (error "PAGE-STRUCTURES :BASE-ADDRESS must be 4 KiB aligned, got ~S." base-address))
          (let ((offset (- base-address section-base)))
            (when (minusp offset)
              (error "PAGE-STRUCTURES :BASE-ADDRESS ~S is below this section's base address ~S."
                     base-address section-base))
            (emit-pad-to state offset 0)))
        (emit-align state 4096))
    (multiple-value-bind (first-directory last-directory)
        (page-directory-index-range descriptor)
      (emit-label state name)
      (ecase granularity
        (:4mib
         (let ((entry-flags (page-flags-mask flags :large-page t)))
           (dotimes (index 1024)
             (if (<= first-directory index last-directory)
                 (emit-dd state (list (+ (* index #x400000) entry-flags)))
                 (emit-dd state '(0))))))
        (:4kb
         (let ((directory-flags (page-flags-mask flags :table-pointer t))
               (page-flags (page-flags-mask flags)))
           (dotimes (index 1024)
             (if (<= first-directory index last-directory)
                 (emit-dd state (list (list :linear+
                                            (page-structure-table-label descriptor index)
                                            directory-flags)))
                 (emit-dd state '(0))))
           (loop for directory-index from first-directory to last-directory do
             (emit-label state (page-structure-table-label descriptor directory-index))
             (dotimes (entry 1024)
               (let ((page-base (+ (* directory-index #x400000) (* entry 4096))))
                 (if (and (>= page-base coverage-start)
                          (<= page-base coverage-end))
                     (emit-dd state (list (+ page-base page-flags)))
                     (emit-dd state '(0))))))))))))

(defun ensure-runtime-address-operand (operand context)
  (let ((operand (resolve-operand context operand)))
    (cond ((or (integerp operand)
               (dword-register-p operand)
               (linear-reference-p operand)
               (linear-addend-reference-p operand))
           operand)
          ((and (symbolp operand)
                (not (register-symbol-p operand))
                (not (keywordp operand)))
           (list :linear operand))
          (t
           (error "Expected an integer, 32-bit register, or linear address reference, got ~S."
                  operand)))))

(defun ensure-page-structure-coverage (descriptor virtual-address form-name)
  (when (integerp virtual-address)
    (unless (page-coverage-contains-p descriptor virtual-address)
      (error "~A virtual address ~S is outside PAGE-STRUCTURES coverage ~S .. ~S."
             form-name
             virtual-address
             (page-structures-descriptor-coverage-start descriptor)
             (page-structures-descriptor-coverage-end descriptor)))))

(defun emit-map-page (environment args)
  (require-bits environment 32 "MAP-PAGE")
  (let* ((state (compile-environment-state environment))
         (descriptor-name (or (first args)
                              (error "MAP-PAGE requires a page-structures name.")))
         (descriptor (lookup-page-structures environment descriptor-name))
         (options (resolve-operands environment (rest args)))
         (virtual (ensure-runtime-address-operand
                   (or (getf options :virtual)
                       (error "MAP-PAGE requires :VIRTUAL."))
                   environment))
         (physical (ensure-runtime-address-operand
                    (or (getf options :physical)
                        (error "MAP-PAGE requires :PHYSICAL."))
                    environment))
         (flags (page-flags-mask (getf options :flags '(:present :writable))
                                 :large-page (eq (page-structures-descriptor-granularity descriptor)
                                                 :4mib)))
         (section-base (section-linear-base (compile-environment-section environment)))
         (done-label (gensym "MAP-PAGE-DONE-")))
    (ensure-page-structure-coverage descriptor virtual "MAP-PAGE")
    (case (page-structures-descriptor-granularity descriptor)
      (:4mib
       (encode-instruction state 'mov (list 'eax virtual))
       (encode-instruction state 'shr '(eax 22))
       (encode-instruction state 'and '(eax #x3FF))
       (encode-instruction state 'mov (list 'ebx descriptor-name))
       (encode-instruction state 'mov (list 'edx physical))
       (encode-instruction state 'and '(edx #xFFC00000))
       (encode-instruction state 'or (list 'edx flags))
       (encode-instruction state 'mov (list (list :MEM :DWORD 'ebx 'eax 4 0) 'edx)))
      (:4kb
       (encode-instruction state 'mov (list 'eax virtual))
       (encode-instruction state 'mov '(edx eax))
       (encode-instruction state 'shr '(eax 22))
       (encode-instruction state 'and '(eax #x3FF))
       (encode-instruction state 'mov '(ecx edx))
       (encode-instruction state 'shr '(ecx 12))
       (encode-instruction state 'and '(ecx #x3FF))
       (encode-instruction state 'mov (list 'ebx descriptor-name))
       (encode-instruction state 'mov (list 'ebx (list :MEM :DWORD 'ebx 'eax 4 0)))
       (encode-instruction state 'test '(ebx ebx))
       (encode-instruction state 'jz (list done-label))
       (encode-instruction state 'and '(ebx #xFFFFF000))
       (encode-instruction state 'sub (list 'ebx section-base))
       (encode-instruction state 'mov (list 'eax physical))
       (encode-instruction state 'and '(eax #xFFFFF000))
       (encode-instruction state 'or (list 'eax flags))
       (encode-instruction state 'mov (list (list :MEM :DWORD 'ebx 'ecx 4 0) 'eax))
       (emit-label state done-label)))))

(defun emit-unmap-page (environment args)
  (require-bits environment 32 "UNMAP-PAGE")
  (let* ((state (compile-environment-state environment))
         (descriptor-name (or (first args)
                              (error "UNMAP-PAGE requires a page-structures name.")))
         (descriptor (lookup-page-structures environment descriptor-name))
         (options (resolve-operands environment (rest args)))
         (virtual (ensure-runtime-address-operand
                   (or (getf options :virtual)
                       (error "UNMAP-PAGE requires :VIRTUAL."))
                   environment))
         (section-base (section-linear-base (compile-environment-section environment)))
         (done-label (gensym "UNMAP-PAGE-DONE-")))
    (ensure-page-structure-coverage descriptor virtual "UNMAP-PAGE")
    (case (page-structures-descriptor-granularity descriptor)
      (:4mib
       (encode-instruction state 'mov (list 'eax virtual))
       (encode-instruction state 'shr '(eax 22))
       (encode-instruction state 'and '(eax #x3FF))
       (encode-instruction state 'mov (list 'ebx descriptor-name))
       (encode-instruction state 'mov (list (list :MEM :DWORD 'ebx 'eax 4 0) 0)))
      (:4kb
       (encode-instruction state 'mov (list 'eax virtual))
       (encode-instruction state 'mov '(edx eax))
       (encode-instruction state 'shr '(eax 22))
       (encode-instruction state 'and '(eax #x3FF))
       (encode-instruction state 'mov '(ecx edx))
       (encode-instruction state 'shr '(ecx 12))
       (encode-instruction state 'and '(ecx #x3FF))
       (encode-instruction state 'mov (list 'ebx descriptor-name))
       (encode-instruction state 'mov (list 'ebx (list :MEM :DWORD 'ebx 'eax 4 0)))
       (encode-instruction state 'test '(ebx ebx))
       (encode-instruction state 'jz (list done-label))
       (encode-instruction state 'and '(ebx #xFFFFF000))
       (encode-instruction state 'sub (list 'ebx section-base))
       (encode-instruction state 'mov (list (list :MEM :DWORD 'ebx 'ecx 4 0) 0))
       (emit-label state done-label)))))

(defun emit-page-table (environment form)
  (let* ((state (compile-environment-state environment))
         (name (second form))
         (options (resolve-operands environment (third form)))
         (start (getf options :start 0))
         (count (getf options :count 1024))
         (page-size (getf options :page-size 4096))
         (flags (getf options :flags #x003)))
    (emit-label state name)
    (dotimes (index count)
      (emit-dd state (list (+ start (* index page-size) flags))))))

(defun emit-page-directory (environment form)
  (let* ((state (compile-environment-state environment))
         (name (second form))
         (options (resolve-operands environment (third form)))
         (entries (cdddr form))
         (count (getf options :count 1024))
         (directory (make-array count :initial-element 0)))
    (dolist (entry entries)
      (destructuring-bind (kind index value &rest entry-options)
          (resolve-operands environment entry)
        (unless (typep index `(integer 0 ,(1- count)))
          (error "Page-directory index ~S is out of range 0..~D." index (1- count)))
        (case (token-keyword kind)
          (:table
           (setf (aref directory index)
                 (list :linear+ value (getf entry-options :flags #x003))))
          (:page4mib
           (unless (integerp value)
             (error "4 MiB page-directory entries currently require an integer base address, got ~S."
                    value))
           (setf (aref directory index)
                 (+ value (getf entry-options :flags #x003) #x80)))
          (:raw
           (setf (aref directory index) value))
          (t
            (error "Unsupported page-directory entry form ~S." kind)))))
    (emit-label state name)
    (dotimes (index count)
      (emit-dd state (list (aref directory index))))))

(defun emit-boot-info (environment form)
  (let* ((state (compile-environment-state environment))
         (name (second form))
         (options (resolve-operands environment (third form)))
         (magic (getf options :magic #x42575431))
         (version (getf options :version 1))
         (gdt (getf options :gdt))
         (idt (getf options :idt))
         (page-directory (getf options :page-directory))
         (entry (getf options :entry))
         (section-base (section-linear-base (compile-environment-section environment))))
    (emit-label state name)
    (emit-dd state (list magic
                         version
                         section-base
                         (if gdt (list :linear gdt) 0)
                         (if idt (list :linear idt) 0)
                         (if page-directory (list :linear page-directory) 0)
                         (if entry (list :linear entry) 0)
                         0))))

(defun helper-string-register (environment)
  (if (= (assembly-bits (compile-environment-state environment)) 16)
      'si
      'esi))

(defun emit-print-helper (environment)
  (let* ((state (compile-environment-state environment))
         (routine-label '__bootwright-bios-print-z)
         (loop-label (gensym "PRINT-LOOP-"))
         (done-label (gensym "PRINT-DONE-")))
    (unless (gethash routine-label (assembly-state-labels state))
      (emit-label state routine-label)
      (encode-instruction state 'cld '())
      (emit-label state loop-label)
      (encode-instruction state 'lodsb '())
      (encode-instruction state 'or '(al al))
      (encode-instruction state 'jz (list done-label))
      (encode-instruction state 'mov '(ah #x0E))
      (encode-instruction state 'int '(#x10))
      (encode-instruction state 'jmp (list loop-label))
      (emit-label state done-label)
      ;; Append CR LF so each BIOS-PRINT produces a complete line.
      (encode-instruction state 'mov '(ah #x0E))
      (encode-instruction state 'mov '(al #x0D))
      (encode-instruction state 'int '(#x10))
      (encode-instruction state 'mov '(ah #x0E))
      (encode-instruction state 'mov '(al #x0A))
      (encode-instruction state 'int '(#x10))
      (encode-instruction state 'ret '()))))

(defun emit-debug-print-helper (environment)
  (let* ((state (compile-environment-state environment))
         (routine-label '__bootwright-debug-print-z)
         (loop-label (gensym "DEBUG-PRINT-LOOP-"))
         (done-label (gensym "DEBUG-PRINT-DONE-")))
    (unless (gethash routine-label (assembly-state-labels state))
      (emit-label state routine-label)
      (encode-instruction state 'cld '())
      (emit-label state loop-label)
      (encode-instruction state 'lodsb '())
      (encode-instruction state 'or '(al al))
      (encode-instruction state 'jz (list done-label))
      (encode-instruction state 'out '(#xE9 al))
      (encode-instruction state 'jmp (list loop-label))
      (emit-label state done-label)
      (encode-instruction state 'ret '()))))

(defun emit-vga-print-helper (environment)
  (let* ((state (compile-environment-state environment))
         (routine-label '__bootwright-vga-print-z)
         (loop-label (gensym "VGA-PRINT-LOOP-"))
         (done-label (gensym "VGA-PRINT-DONE-")))
    (unless (gethash routine-label (assembly-state-labels state))
      (emit-label state routine-label)
      (encode-instruction state 'cld '())
      (emit-label state loop-label)
      (encode-instruction state 'lodsb '())
      (encode-instruction state 'or '(al al))
      (encode-instruction state 'jz (list done-label))
      (encode-instruction state 'stosw '())
      (encode-instruction state 'jmp (list loop-label))
      (emit-label state done-label)
      (encode-instruction state 'ret '()))))

(defun emit-vga-emit-hex-nibble-helper (environment)
  (let* ((state (compile-environment-state environment))
         (routine-label '__bootwright-vga-emit-hex-nibble)
         (digit-label (gensym "HEX-NIBBLE-DIGIT-")))
    (unless (gethash routine-label (assembly-state-labels state))
      (emit-label state routine-label)
      (encode-instruction state 'cmp '(dl 10))
      (encode-instruction state 'jb (list digit-label))
      (encode-instruction state 'add '(dl 7))
      (emit-label state digit-label)
      (encode-instruction state 'add '(dl 48))
      (encode-instruction state 'mov '(al dl))
      (encode-instruction state 'stosw '())
      (encode-instruction state 'ret '()))))

(defun emit-vga-print-decimal-helper (environment)
  ;; In: EBX = value, EDI = dst offset, AH = attribute.
  ;; Always emits 10 ASCII digits, zero-padded.
  ;; Saves: EAX, EBX, ECX, EDX, EBP.
  (let* ((state (compile-environment-state environment))
         (routine-label '__bootwright-vga-print-decimal)
         (div-loop-label (gensym "DEC-DIV-"))
         (print-loop-label (gensym "DEC-PRINT-")))
    (unless (gethash routine-label (assembly-state-labels state))
      (emit-label state routine-label)
      (encode-instruction state 'push '(eax))
      (encode-instruction state 'push '(ebx))
      (encode-instruction state 'push '(ecx))
      (encode-instruction state 'push '(edx))
      (encode-instruction state 'push '(ebp))
      ;; Stash the attribute byte in BP[15:8] so we can recover AH after div.
      (encode-instruction state 'xor '(al al))
      (encode-instruction state 'mov '(bp ax))
      ;; Set up the dividend / divisor / digit count.
      (encode-instruction state 'mov '(eax ebx))
      (encode-instruction state 'mov '(ecx 10))
      (encode-instruction state 'mov '(ebx 10))
      (emit-label state div-loop-label)
      (encode-instruction state 'xor '(edx edx))
      (encode-instruction state 'div '(ecx))
      (encode-instruction state 'push '(edx))
      (encode-instruction state 'dec '(ebx))
      (encode-instruction state 'jnz (list div-loop-label))
      ;; AH was clobbered by the divisions; restore it from BP.
      (encode-instruction state 'mov '(ax bp))
      (encode-instruction state 'mov '(ebx 10))
      (emit-label state print-loop-label)
      (encode-instruction state 'pop '(edx))
      (encode-instruction state 'add '(dl 48))
      (encode-instruction state 'mov '(al dl))
      (encode-instruction state 'stosw '())
      (encode-instruction state 'dec '(ebx))
      (encode-instruction state 'jnz (list print-loop-label))
      (encode-instruction state 'pop '(ebp))
      (encode-instruction state 'pop '(edx))
      (encode-instruction state 'pop '(ecx))
      (encode-instruction state 'pop '(ebx))
      (encode-instruction state 'pop '(eax))
      (encode-instruction state 'ret '()))))

(defun emit-vga-print-ascii4-helper (environment)
  ;; In: EBX = 4 ASCII bytes (LSB first), EDI = dst offset, AH = attribute.
  ;; Saves: EBX, EDX (helper preserves them across the call).
  (let* ((state (compile-environment-state environment))
         (routine-label '__bootwright-vga-print-ascii4))
    (unless (gethash routine-label (assembly-state-labels state))
      (emit-label state routine-label)
      (encode-instruction state 'push '(edx))
      (encode-instruction state 'mov '(edx ebx))
      (dotimes (i 4)
        (when (plusp i)
          (encode-instruction state 'shr '(edx 8)))
        (encode-instruction state 'mov '(al dl))
        (encode-instruction state 'stosw '()))
      (encode-instruction state 'pop '(edx))
      (encode-instruction state 'ret '()))))

(defun emit-vga-print-hex8-helper (environment)
  (let* ((state (compile-environment-state environment))
         (routine-label '__bootwright-vga-print-hex8))
    (unless (gethash routine-label (assembly-state-labels state))
      (emit-vga-emit-hex-nibble-helper environment)
      (emit-label state routine-label)
      (encode-instruction state 'push '(edx))
      (dolist (shift '(28 24 20 16 12 8 4 0))
        (encode-instruction state 'mov '(edx ebx))
        (when (plusp shift)
          (encode-instruction state 'shr (list 'edx shift)))
        (unless (= shift 28)
          (encode-instruction state 'and '(dl 15)))
        (encode-instruction state 'call (list '__bootwright-vga-emit-hex-nibble)))
      (encode-instruction state 'pop '(edx))
      (encode-instruction state 'ret '()))))

(defun emit-serial-print-helper (environment)
  ;; Preserves EAX/EBX/ECX/EDX across each character; only ESI advances.
  ;; The character is parked on the stack while we poll LSR, so EBX is
  ;; safe to use as a caller-supplied counter (e.g. inside an IRQ handler).
  (let* ((state (compile-environment-state environment))
         (routine-label '__bootwright-serial-print-z)
         (loop-label (gensym "SERIAL-PRINT-LOOP-"))
         (wait-label (gensym "SERIAL-PRINT-WAIT-"))
         (done-label (gensym "SERIAL-PRINT-DONE-")))
    (unless (gethash routine-label (assembly-state-labels state))
      (emit-label state routine-label)
      (encode-instruction state 'cld '())
      (emit-label state loop-label)
      (encode-instruction state 'lodsb '())
      (encode-instruction state 'or '(al al))
      (encode-instruction state 'jz (list done-label))
      (encode-instruction state 'push '(eax))
      (emit-label state wait-label)
      (encode-instruction state 'mov '(dx #x3FD))
      (encode-instruction state 'in '(al dx))
      (encode-instruction state 'test '(al #x20))
      (encode-instruction state 'jz (list wait-label))
      (encode-instruction state 'pop '(eax))
      (encode-instruction state 'mov '(dx #x3F8))
      (encode-instruction state 'out '(dx al))
      (encode-instruction state 'jmp (list loop-label))
      (emit-label state done-label)
      (encode-instruction state 'ret '()))))

(defun emit-required-helpers (environment)
  (let ((state (compile-environment-state environment))
        (helpers (compile-environment-helpers environment)))
    (dolist (helper '(:bios-print-z :vga-print-z :debug-print-z
                      :vga-print-hex8 :vga-print-ascii4 :vga-print-decimal
                      :serial-print-z))
      (let ((bits (gethash helper helpers)))
        (when bits
          (set-assembly-bits state bits)
          (ecase helper
            (:bios-print-z
             (emit-print-helper environment))
            (:vga-print-z
             (emit-vga-print-helper environment))
            (:debug-print-z
             (emit-debug-print-helper environment))
            (:vga-print-hex8
             (emit-vga-print-hex8-helper environment))
            (:vga-print-ascii4
             (emit-vga-print-ascii4-helper environment))
            (:vga-print-decimal
             (emit-vga-print-decimal-helper environment))
            (:serial-print-z
             (emit-serial-print-helper environment)))
          (remhash helper helpers))))))

(defun print-label-designator (designator)
  (cond ((symbolp designator)
         designator)
        ((label-reference-p designator)
         (second designator))
        (t
         (error "String output forms expect a literal string or label, got ~S." designator))))

(defun emit-bios-print (environment string-designator)
  (let* ((state (compile-environment-state environment))
         (skip-label (gensym "AFTER-STRING-"))
         (string-label (and (stringp string-designator)
                            (gensym "PRINT-STRING-"))))
    (mark-helper-needed environment :bios-print-z)
    (when string-label
      (encode-instruction state 'jmp (list skip-label))
      (emit-label state string-label)
      (emit-ascii-z state string-designator)
      (emit-label state skip-label))
    (encode-instruction state 'mov (list 'si (or string-label
                                              (print-label-designator string-designator))))
    (encode-instruction state 'call (list '__bootwright-bios-print-z))))

(defun emit-bios-set-cursor (environment args)
  (require-bits environment 16 "BIOS-SET-CURSOR")
  (let* ((state (compile-environment-state environment))
         (args (resolve-operands environment args))
         (row (getf args :row 0))
         (column (getf args :column 0))
         (page (getf args :page 0)))
    (encode-instruction state 'mov '(ah #x02))
    (encode-instruction state 'mov (list 'bh page))
    (encode-instruction state 'mov (list 'dh row))
    (encode-instruction state 'mov (list 'dl column))
    (encode-instruction state 'int '(#x10))))

(defun emit-bios-print-at (environment string-designator args)
  (emit-bios-set-cursor environment args)
  (emit-bios-print environment string-designator))

(defun emit-debug-print (environment string-designator)
  (let* ((state (compile-environment-state environment))
         (skip-label (gensym "AFTER-DEBUG-STRING-"))
         (string-label (and (stringp string-designator)
                            (gensym "DEBUG-STRING-")))
         (string-register (helper-string-register environment)))
    (mark-helper-needed environment :debug-print-z)
    (when string-label
      (encode-instruction state 'jmp (list skip-label))
      (emit-label state string-label)
      (emit-ascii-z state string-designator)
      (emit-label state skip-label))
    (encode-instruction state 'mov (list string-register
                                        (or string-label
                                            (print-label-designator string-designator))))
    (encode-instruction state 'call (list '__bootwright-debug-print-z))))

(defun emit-vga-print (environment string-designator args)
  (require-bits environment 32 "VGA-PRINT")
  (let* ((state (compile-environment-state environment))
         (skip-label (gensym "AFTER-VGA-STRING-"))
         (args (resolve-operands environment args))
         (string-label (and (stringp string-designator)
                            (gensym "VGA-STRING-")))
         (attribute (getf args :attribute #x1F))
         (address (getf args :address #xB8000))
         (section-base (section-linear-base (compile-environment-section environment)))
         (video-offset (- address section-base)))
    (when (minusp video-offset)
      (error "VGA address #x~X is below the current section base #x~X." address section-base))
    (mark-helper-needed environment :vga-print-z)
    (when string-label
      (encode-instruction state 'jmp (list skip-label))
      (emit-label state string-label)
      (emit-ascii-z state string-designator)
      (emit-label state skip-label))
    (encode-instruction state 'mov (list 'edi video-offset))
    (encode-instruction state 'mov (list 'esi (or string-label
                                               (print-label-designator string-designator))))
    (encode-instruction state 'mov (list 'ah attribute))
    (encode-instruction state 'call (list '__bootwright-vga-print-z))))

(defun emit-vga-print-at (environment string-designator args)
  (let* ((args (resolve-operands environment args))
         (address (resolve-vga-address args)))
    (emit-vga-print environment string-designator
                    (list :address address
                          :attribute (getf args :attribute #x1F)))))

(defun make-box-top-line (width args)
  (let* ((horizontal (getf args :horizontal #\-))
         (top-left (getf args :top-left #\+))
         (top-right (getf args :top-right #\+))
         (interior-width (- width 2))
         (title (getf args :title)))
    (concatenate
     'string
     (string (ascii-display-char top-left))
     (if (and title (> interior-width 0))
         (let* ((trimmed (subseq title 0 (min (length title) (max 0 (- interior-width 2)))))
                (content (if (zerop (length trimmed))
                             ""
                             (concatenate 'string " " trimmed " ")))
                (left-fill (max 0 (floor (- interior-width (length content)) 2)))
                (right-fill (max 0 (- interior-width (length content) left-fill))))
           (concatenate 'string
                        (make-filled-string left-fill horizontal)
                        content
                        (make-filled-string right-fill horizontal)))
         (make-filled-string interior-width horizontal))
     (string (ascii-display-char top-right)))))

(defun make-box-middle-line (width args)
  (let* ((fill (getf args :fill #\Space))
         (vertical (getf args :vertical #\|)))
    (concatenate 'string
                 (string (ascii-display-char vertical))
                 (make-filled-string (- width 2) fill)
                 (string (ascii-display-char vertical)))))

(defun make-box-bottom-line (width args)
  (let* ((horizontal (getf args :horizontal #\-))
         (bottom-left (getf args :bottom-left #\+))
         (bottom-right (getf args :bottom-right #\+)))
    (concatenate 'string
                 (string (ascii-display-char bottom-left))
                 (make-filled-string (- width 2) horizontal)
                 (string (ascii-display-char bottom-right)))))

(defun emit-vga-box (environment args)
  (require-bits environment 32 "VGA-BOX")
  (let* ((args (resolve-operands environment args))
         (row (getf args :row 0))
         (column (getf args :column 0))
         (width (getf args :width))
         (height (getf args :height))
         (attribute (getf args :attribute #x1F))
         (columns (getf args :columns 80))
         (base-address (getf args :base-address #xB8000)))
    (unless (and (integerp width) (>= width 2))
      (error "VGA-BOX requires :WIDTH >= 2, got ~S." width))
    (unless (and (integerp height) (>= height 2))
      (error "VGA-BOX requires :HEIGHT >= 2, got ~S." height))
    (emit-vga-print-at environment
                       (make-box-top-line width args)
                       (list :row row
                             :column column
                             :columns columns
                             :base-address base-address
                             :attribute attribute))
    (loop for current-row from (1+ row) below (+ row (1- height)) do
      (emit-vga-print-at environment
                         (make-box-middle-line width args)
                         (list :row current-row
                               :column column
                               :columns columns
                               :base-address base-address
                               :attribute attribute)))
    (emit-vga-print-at environment
                       (make-box-bottom-line width args)
                       (list :row (+ row (1- height))
                             :column column
                             :columns columns
                             :base-address base-address
                             :attribute attribute))))

(defun emit-load-gdt (environment args)
  (let ((pointer (resolve-operand environment
                                  (or (first args)
                                      (error "LOAD-GDT requires the GDT descriptor label.")))))
    (encode-instruction (compile-environment-state environment) 'lgdt (list pointer))))

(defun emit-io-wait (environment)
  (let ((state (compile-environment-state environment)))
    (encode-instruction state 'mov '(al 0))
    (encode-instruction state 'out '(#x80 al))))

(defun emit-set-video-mode (environment args)
  (require-bits environment 16 "SET-VIDEO-MODE")
  (let* ((state (compile-environment-state environment))
         (mode (resolve-operand environment (or (first args) 3))))
    (encode-instruction state 'mov '(ah 0))
    (encode-instruction state 'mov (list 'al mode))
    (encode-instruction state 'int '(#x10))))

(defun emit-enable-a20-fast (environment)
  (let ((state (compile-environment-state environment)))
    (encode-instruction state 'in '(al #x92))
    (encode-instruction state 'and '(al #xFE))
    (encode-instruction state 'or '(al #x02))
    (encode-instruction state 'out '(#x92 al))))

(defun emit-clear-vga (environment args)
  (require-bits environment 32 "CLEAR-VGA")
  (let* ((state (compile-environment-state environment))
         (args (resolve-operands environment args))
         (address (getf args :address #xB8000))
         (cells (getf args :cells (* 80 25)))
         (attribute (getf args :attribute #x07))
         (character (getf args :char 32))
         (section-base (section-linear-base (compile-environment-section environment)))
         (video-offset (- address section-base))
         (loop-label (gensym "CLEAR-VGA-")))
    (when (minusp video-offset)
      (error "VGA address #x~X is below the current section base #x~X." address section-base))
    (encode-instruction state 'cld '())
    (encode-instruction state 'mov (list 'edi video-offset))
    (encode-instruction state 'mov (list 'ecx cells))
    (encode-instruction state 'mov (list 'ah attribute))
    (encode-instruction state 'mov (list 'al character))
    (emit-label state loop-label)
    (encode-instruction state 'stosw '())
    (encode-instruction state 'dec '(ecx))
    (encode-instruction state 'jnz (list loop-label))))

(defun emit-framebuffer-clear (environment framebuffer-designator args)
  (let* ((framebuffer (resolve-framebuffer-device environment framebuffer-designator))
         (cells (* (vga-text-framebuffer-columns framebuffer)
                   (vga-text-framebuffer-rows framebuffer))))
    (emit-clear-vga environment
                    (list* :address (vga-text-framebuffer-base-address framebuffer)
                           :cells cells
                           :attribute (getf args :attribute
                                            (vga-text-framebuffer-default-attribute framebuffer))
                           args))))

(defun emit-framebuffer-print-at (environment framebuffer-designator string-designator args)
  (let ((framebuffer (resolve-framebuffer-device environment framebuffer-designator)))
    (emit-vga-print-at environment
                       string-designator
                       (list* :columns (vga-text-framebuffer-columns framebuffer)
                              :base-address (vga-text-framebuffer-base-address framebuffer)
                              :attribute (getf args :attribute
                                               (vga-text-framebuffer-default-attribute framebuffer))
                              args))))

(defun emit-framebuffer-box (environment framebuffer-designator args)
  (let ((framebuffer (resolve-framebuffer-device environment framebuffer-designator)))
    (emit-vga-box environment
                  (list* :columns (vga-text-framebuffer-columns framebuffer)
                         :base-address (vga-text-framebuffer-base-address framebuffer)
                         :attribute (getf args :attribute
                                          (vga-text-framebuffer-default-attribute framebuffer))
                         args))))

(defun resolve-framebuffer-region (framebuffer args form-name)
  (let* ((row (getf args :row 0))
         (column (getf args :column 0))
         (width (getf args :width))
         (height (getf args :height))
         (columns (vga-text-framebuffer-columns framebuffer))
         (rows (vga-text-framebuffer-rows framebuffer)))
    (unless (and (integerp row) (<= 0 row))
      (error "~A :ROW must be a non-negative integer, got ~S." form-name row))
    (unless (and (integerp column) (<= 0 column))
      (error "~A :COLUMN must be a non-negative integer, got ~S." form-name column))
    (unless (and (integerp width) (plusp width))
      (error "~A :WIDTH must be a positive integer, got ~S." form-name width))
    (unless (and (integerp height) (plusp height))
      (error "~A :HEIGHT must be a positive integer, got ~S." form-name height))
    (when (> (+ column width) columns)
      (error "~A region (~D,~D ~Dx~D) exceeds the framebuffer width of ~D columns."
             form-name row column width height columns))
    (when (> (+ row height) rows)
      (error "~A region (~D,~D ~Dx~D) exceeds the framebuffer height of ~D rows."
             form-name row column width height rows))
    (values row column width height columns rows)))

(defun emit-framebuffer-fill-region (environment framebuffer-designator args)
  (require-bits environment 32 "FRAMEBUFFER-FILL-REGION")
  (let* ((state (compile-environment-state environment))
         (framebuffer (resolve-framebuffer-device environment framebuffer-designator))
         (args (resolve-operands environment args))
         (attribute (getf args :attribute
                          (vga-text-framebuffer-default-attribute framebuffer)))
         (character (let ((designator (getf args :char 32)))
                      (if (integerp designator)
                          designator
                          (char-code (ascii-display-char designator)))))
         (base-address (vga-text-framebuffer-base-address framebuffer))
         (section-base (section-linear-base (compile-environment-section environment)))
         (row-loop (gensym "FB-FILL-ROW-"))
         (cell-loop (gensym "FB-FILL-CELL-"))
         (next-row (gensym "FB-FILL-NEXT-"))
         (done (gensym "FB-FILL-DONE-")))
    (multiple-value-bind (row column width height columns _rows)
        (resolve-framebuffer-region framebuffer args "FRAMEBUFFER-FILL-REGION")
      (declare (ignore _rows))
      (unless (and (integerp attribute) (typep attribute '(integer 0 255)))
        (error "FRAMEBUFFER-FILL-REGION :ATTRIBUTE must fit in one byte, got ~S."
               attribute))
      (unless (and (integerp character) (typep character '(integer 0 255)))
        (error "FRAMEBUFFER-FILL-REGION :CHAR must fit in one byte, got ~S." character))
      (let* ((region-offset (+ (- base-address section-base)
                               (* 2 (+ column (* row columns)))))
             (row-skip-bytes (* 2 (- columns width))))
        (when (minusp region-offset)
          (error "Framebuffer address #x~X is below the current section base #x~X."
                 base-address section-base))
        (encode-instruction state 'cld '())
        (encode-instruction state 'mov (list 'edi region-offset))
        (encode-instruction state 'mov (list 'edx height))
        (encode-instruction state 'mov (list 'ah attribute))
        (encode-instruction state 'mov (list 'al character))
        (emit-label state row-loop)
        (encode-instruction state 'test '(edx edx))
        (encode-instruction state 'jz (list done))
        (encode-instruction state 'mov (list 'ecx width))
        (emit-label state cell-loop)
        (encode-instruction state 'test '(ecx ecx))
        (encode-instruction state 'jz (list next-row))
        (encode-instruction state 'stosw '())
        (encode-instruction state 'dec '(ecx))
        (encode-instruction state 'jmp (list cell-loop))
        (emit-label state next-row)
        (when (plusp row-skip-bytes)
          (encode-instruction state 'add (list 'edi row-skip-bytes)))
        (encode-instruction state 'dec '(edx))
        (encode-instruction state 'jmp (list row-loop))
        (emit-label state done)))))

(defun emit-framebuffer-scroll-region (environment framebuffer-designator args)
  (require-bits environment 32 "FRAMEBUFFER-SCROLL-REGION")
  (let* ((state (compile-environment-state environment))
         (framebuffer (resolve-framebuffer-device environment framebuffer-designator))
         (args (resolve-operands environment args))
         (attribute (getf args :attribute
                          (vga-text-framebuffer-default-attribute framebuffer)))
         (character (let ((designator (getf args :char 32)))
                      (if (integerp designator)
                          designator
                          (char-code (ascii-display-char designator)))))
         (lines (getf args :lines 1))
         (base-address (vga-text-framebuffer-base-address framebuffer))
         (section-base (section-linear-base (compile-environment-section environment)))
         (copy-row-loop (gensym "FB-REGION-SCROLL-COPY-ROW-"))
         (copy-cell-loop (gensym "FB-REGION-SCROLL-COPY-CELL-"))
         (copy-next-row (gensym "FB-REGION-SCROLL-COPY-NEXT-"))
         (copy-done (gensym "FB-REGION-SCROLL-COPY-DONE-"))
         (clear-row-loop (gensym "FB-REGION-SCROLL-CLEAR-ROW-"))
         (clear-cell-loop (gensym "FB-REGION-SCROLL-CLEAR-CELL-"))
         (clear-next-row (gensym "FB-REGION-SCROLL-CLEAR-NEXT-"))
         (done (gensym "FB-REGION-SCROLL-DONE-")))
    (multiple-value-bind (row column width height columns _rows)
        (resolve-framebuffer-region framebuffer args "FRAMEBUFFER-SCROLL-REGION")
      (declare (ignore _rows))
      (unless (and (integerp lines) (<= 0 lines height))
        (error "FRAMEBUFFER-SCROLL-REGION :LINES must be an integer from 0 to ~D, got ~S."
               height lines))
      (unless (and (integerp attribute) (typep attribute '(integer 0 255)))
        (error "FRAMEBUFFER-SCROLL-REGION :ATTRIBUTE must fit in one byte, got ~S."
               attribute))
      (unless (and (integerp character) (typep character '(integer 0 255)))
        (error "FRAMEBUFFER-SCROLL-REGION :CHAR must fit in one byte, got ~S." character))
      (let* ((region-offset (+ (- base-address section-base)
                               (* 2 (+ column (* row columns)))))
             (row-skip-bytes (* 2 (- columns width)))
             (moved-rows (- height lines))
             (source-offset (+ region-offset (* 2 (* lines columns))))
             (clear-offset (+ region-offset (* 2 (* moved-rows columns)))))
        (when (minusp region-offset)
          (error "Framebuffer address #x~X is below the current section base #x~X."
                 base-address section-base))
        (encode-instruction state 'cld '())
        (encode-instruction state 'mov (list 'esi source-offset))
        (encode-instruction state 'mov (list 'edi region-offset))
        (encode-instruction state 'mov (list 'edx moved-rows))
        (emit-label state copy-row-loop)
        (encode-instruction state 'test '(edx edx))
        (encode-instruction state 'jz (list copy-done))
        (encode-instruction state 'mov (list 'ecx width))
        (emit-label state copy-cell-loop)
        (encode-instruction state 'test '(ecx ecx))
        (encode-instruction state 'jz (list copy-next-row))
        (encode-instruction state 'mov '(ax (:mem :word esi)))
        (encode-instruction state 'mov '((:mem :word edi) ax))
        (encode-instruction state 'add '(esi 2))
        (encode-instruction state 'add '(edi 2))
        (encode-instruction state 'dec '(ecx))
        (encode-instruction state 'jmp (list copy-cell-loop))
        (emit-label state copy-next-row)
        (when (plusp row-skip-bytes)
          (encode-instruction state 'add (list 'esi row-skip-bytes))
          (encode-instruction state 'add (list 'edi row-skip-bytes)))
        (encode-instruction state 'dec '(edx))
        (encode-instruction state 'jmp (list copy-row-loop))
        (emit-label state copy-done)
        (encode-instruction state 'mov (list 'edi clear-offset))
        (encode-instruction state 'mov (list 'edx lines))
        (encode-instruction state 'mov (list 'ah attribute))
        (encode-instruction state 'mov (list 'al character))
        (emit-label state clear-row-loop)
        (encode-instruction state 'test '(edx edx))
        (encode-instruction state 'jz (list done))
        (encode-instruction state 'mov (list 'ecx width))
        (emit-label state clear-cell-loop)
        (encode-instruction state 'test '(ecx ecx))
        (encode-instruction state 'jz (list clear-next-row))
        (encode-instruction state 'stosw '())
        (encode-instruction state 'dec '(ecx))
        (encode-instruction state 'jmp (list clear-cell-loop))
        (emit-label state clear-next-row)
        (when (plusp row-skip-bytes)
          (encode-instruction state 'add (list 'edi row-skip-bytes)))
        (encode-instruction state 'dec '(edx))
        (encode-instruction state 'jmp (list clear-row-loop))
        (emit-label state done)))))

(defun emit-framebuffer-window (environment framebuffer-designator args)
  (require-bits environment 32 "FRAMEBUFFER-WINDOW")
  (let* ((framebuffer (resolve-framebuffer-device environment framebuffer-designator))
         (args (resolve-operands environment args))
         (border-attribute (getf args :border-attribute
                                 (vga-text-framebuffer-default-attribute framebuffer)))
         (fill-attribute (getf args :fill-attribute
                               (vga-text-framebuffer-default-attribute framebuffer)))
         (title-attribute (getf args :title-attribute border-attribute))
         (fill-char (getf args :fill-char 32))
         (title-fill-char (getf args :title-fill-char fill-char))
         (vertical (getf args :vertical #\|))
         (title (getf args :title))
         (title-column (getf args :title-column 2))
         (border-args (copy-list args)))
    (multiple-value-bind (row column width height _columns _rows)
        (resolve-framebuffer-region framebuffer args "FRAMEBUFFER-WINDOW")
      (declare (ignore _columns _rows))
      (unless (>= width 4)
        (error "FRAMEBUFFER-WINDOW :WIDTH must be at least 4, got ~S." width))
      (unless (>= height 3)
        (error "FRAMEBUFFER-WINDOW :HEIGHT must be at least 3, got ~S." height))
      (remf border-args :title)
      (emit-framebuffer-fill-region environment framebuffer-designator
                                    (list :row (1+ row)
                                          :column (1+ column)
                                          :width (- width 2)
                                          :height (- height 2)
                                          :attribute fill-attribute
                                          :char fill-char))
      (when title
        (emit-framebuffer-fill-region environment framebuffer-designator
                                      (list :row (1+ row)
                                            :column (1+ column)
                                            :width (- width 2)
                                            :height 1
                                            :attribute title-attribute
                                            :char title-fill-char)))
      (emit-framebuffer-print-at environment
                                 framebuffer-designator
                                 (make-box-top-line width border-args)
                                 (list :row row
                                       :column column
                                       :attribute border-attribute))
      (emit-framebuffer-print-at environment
                                 framebuffer-designator
                                 (make-box-bottom-line width border-args)
                                 (list :row (+ row (1- height))
                                       :column column
                                       :attribute border-attribute))
      (emit-framebuffer-fill-region environment framebuffer-designator
                                    (list :row (1+ row)
                                          :column column
                                          :width 1
                                          :height (- height 2)
                                          :attribute border-attribute
                                          :char vertical))
      (emit-framebuffer-fill-region environment framebuffer-designator
                                    (list :row (1+ row)
                                          :column (+ column (1- width))
                                          :width 1
                                          :height (- height 2)
                                          :attribute border-attribute
                                          :char vertical))
      (when title
        (emit-framebuffer-print-at environment
                                   framebuffer-designator
                                   title
                                   (list :row (1+ row)
                                         :column (+ column title-column)
                                         :attribute title-attribute))))))

(defun emit-framebuffer-scroll (environment framebuffer-designator args)
  (require-bits environment 32 "FRAMEBUFFER-SCROLL")
  (let* ((state (compile-environment-state environment))
         (framebuffer (resolve-framebuffer-device environment framebuffer-designator))
         (args (resolve-operands environment args))
         (columns (vga-text-framebuffer-columns framebuffer))
         (rows (vga-text-framebuffer-rows framebuffer))
         (lines (getf args :lines 1))
         (attribute (getf args :attribute
                          (vga-text-framebuffer-default-attribute framebuffer)))
         (character (getf args :char 32))
         (base-address (vga-text-framebuffer-base-address framebuffer))
         (section-base (section-linear-base (compile-environment-section environment)))
         (base-offset (- base-address section-base))
         (moved-cells (* columns (max 0 (- rows lines))))
         (clear-cells (* columns lines))
         (source-offset (+ base-offset (* lines columns 2)))
         (clear-offset (+ base-offset (* moved-cells 2)))
         (copy-loop (gensym "FB-SCROLL-COPY-"))
         (copy-done (gensym "FB-SCROLL-COPY-DONE-"))
         (clear-loop (gensym "FB-SCROLL-CLEAR-"))
         (clear-done (gensym "FB-SCROLL-CLEAR-DONE-")))
    (unless (and (integerp lines) (<= 0 lines rows))
      (error "FRAMEBUFFER-SCROLL :LINES must be an integer from 0 to ~D, got ~S."
             rows lines))
    (when (minusp base-offset)
      (error "Framebuffer address #x~X is below the current section base #x~X."
             base-address section-base))
    (unless (and (integerp attribute) (typep attribute '(integer 0 255)))
      (error "FRAMEBUFFER-SCROLL :ATTRIBUTE must fit in one byte, got ~S." attribute))
    (unless (and (integerp character) (typep character '(integer 0 255)))
      (error "FRAMEBUFFER-SCROLL :CHAR must fit in one byte, got ~S." character))
    (encode-instruction state 'cld '())
    (encode-instruction state 'mov (list 'esi source-offset))
    (encode-instruction state 'mov (list 'edi base-offset))
    (encode-instruction state 'mov (list 'ecx moved-cells))
    (emit-label state copy-loop)
    (encode-instruction state 'test '(ecx ecx))
    (encode-instruction state 'jz (list copy-done))
    (encode-instruction state 'mov '(ax (:mem :word esi)))
    (encode-instruction state 'mov '((:mem :word edi) ax))
    (encode-instruction state 'add '(esi 2))
    (encode-instruction state 'add '(edi 2))
    (encode-instruction state 'dec '(ecx))
    (encode-instruction state 'jmp (list copy-loop))
    (emit-label state copy-done)
    (encode-instruction state 'mov (list 'edi clear-offset))
    (encode-instruction state 'mov (list 'ecx clear-cells))
    (encode-instruction state 'mov (list 'ah attribute))
    (encode-instruction state 'mov (list 'al character))
    (emit-label state clear-loop)
    (encode-instruction state 'test '(ecx ecx))
    (encode-instruction state 'jz (list clear-done))
    (encode-instruction state 'stosw '())
    (encode-instruction state 'dec '(ecx))
    (encode-instruction state 'jmp (list clear-loop))
    (emit-label state clear-done)))

(defun emit-hang (environment)
  (let* ((state (compile-environment-state environment))
         (loop-label (gensym "HANG-")))
    (encode-instruction state 'cli '())
    (emit-label state loop-label)
    (encode-instruction state 'jmp (list loop-label))))

(defun emit-idle-forever (environment)
  (let* ((state (compile-environment-state environment))
         (loop-label (gensym "IDLE-")))
    (encode-instruction state 'cli '())
    (emit-label state loop-label)
    (encode-instruction state 'hlt '())
    (encode-instruction state 'jmp (list loop-label))))

(defun emit-real-mode-prologue (environment args)
  (let* ((state (compile-environment-state environment))
         (args (resolve-operands environment args))
         (section (compile-environment-section environment))
         (segment (or (section-spec-load-segment section) 0))
         (stack (getf args :stack #x7C00)))
    (encode-instruction state 'cli '())
    (encode-instruction state 'mov (list 'ax segment))
    (encode-instruction state 'mov '(ds ax))
    (encode-instruction state 'mov '(es ax))
    (encode-instruction state 'mov '(ss ax))
    (encode-instruction state 'mov (list 'sp stack))
    (encode-instruction state 'cld '())))

(defun emit-protected-mode-prologue (environment args)
  (require-bits environment 32 "INITIALIZE-PROTECTED-MODE")
  (let* ((state (compile-environment-state environment))
         (args (resolve-operands environment args))
         (section-base (section-linear-base (compile-environment-section environment)))
         (data-selector (getf args :data-selector #x10))
         (stack-linear (getf args :stack-linear))
         (stack (if stack-linear
                    (- stack-linear section-base)
                    (getf args :stack #x8000))))
    (when (minusp stack)
      (error "Protected-mode stack #x~X is below the current section base #x~X."
             stack-linear section-base))
    (encode-instruction state 'mov (list 'ax data-selector))
    (encode-instruction state 'mov '(ds ax))
    (encode-instruction state 'mov '(es ax))
    (encode-instruction state 'mov '(fs ax))
    (encode-instruction state 'mov '(gs ax))
    (encode-instruction state 'mov '(ss ax))
    (encode-instruction state 'mov (list 'esp stack))
    (encode-instruction state 'cld '())))

(defun emit-enter-protected-mode (environment args)
  (require-bits environment 16 "ENTER-PROTECTED-MODE")
  (let* ((state (compile-environment-state environment))
         (args (resolve-operands environment args))
         (gdt (or (getf args :gdt)
                  (error "ENTER-PROTECTED-MODE requires :GDT.")))
         (entry (or (getf args :entry)
                    (error "ENTER-PROTECTED-MODE requires :ENTRY.")))
         (boot-info (getf args :boot-info))
         (code-selector (getf args :code-selector #x08))
         (pe-mask (getf args :pe-mask 1)))
    (when boot-info
      (encode-instruction state 'mov (list 'esi boot-info)))
    (encode-instruction state 'lgdt (list gdt))
    (encode-instruction state 'mov '(eax cr0))
    (encode-instruction state 'or (list 'eax pe-mask))
    (encode-instruction state 'mov '(cr0 eax))
    (encode-instruction state 'ljmp32 (list entry code-selector))))

(defun emit-enable-paging (environment args)
  (require-bits environment 32 "ENABLE-PAGING")
  (let* ((state (compile-environment-state environment))
         (args (resolve-operands environment args))
         (page-directory (or (getf args :page-directory)
                             (error "ENABLE-PAGING requires :PAGE-DIRECTORY.")))
         (cr4-mask (getf args :cr4-mask 0))
         (paging-mask (getf args :mask #x80000000))
         (flush-label (gensym "PAGING-ENABLED-")))
    (when (not (zerop cr4-mask))
      (encode-instruction state 'mov '(eax cr4))
      (encode-instruction state 'or (list 'eax cr4-mask))
      (encode-instruction state 'mov '(cr4 eax)))
    (encode-instruction state 'mov (list 'eax (list :linear page-directory)))
    (encode-instruction state 'mov '(cr3 eax))
    (encode-instruction state 'mov '(eax cr0))
    (encode-instruction state 'or (list 'eax paging-mask))
    (encode-instruction state 'mov '(cr0 eax))
    (encode-instruction state 'jmp (list flush-label))
    (emit-label state flush-label)))

(defun emit-load-idt (environment args)
  (let ((pointer (resolve-operand environment
                                  (or (first args)
                                      (error "LOAD-IDT requires the IDT descriptor label.")))))
    (encode-instruction (compile-environment-state environment) 'lidt (list pointer))))

(defun emit-memory-barrier (environment args)
  (let* ((kind (and args (resolve-operand environment (first args))))
         (state (compile-environment-state environment))
         (selector (cond ((null kind) :full)
                         ((symbolp kind) (token-keyword kind))
                         (t kind))))
    (require-bits environment 32 "MEMORY-BARRIER")
    (case selector
      (:full  (encode-instruction state 'mfence '()))
      (:load  (encode-instruction state 'lfence '()))
      (:store (encode-instruction state 'sfence '()))
      (t
       (error "MEMORY-BARRIER expects :LOAD, :STORE, or no argument, got ~S." kind)))))

(defun emit-tlb-flush-page (environment args)
  (require-bits environment 32 "TLB-FLUSH-PAGE")
  (let* ((address (resolve-operand environment
                                   (or (first args)
                                       (error "TLB-FLUSH-PAGE requires a linear address."))))
         (state (compile-environment-state environment))
         (memory-operand (cond ((memory-operand-p address) address)
                               ((or (integerp address) (symbolp address))
                                (list :MEM address))
                               (t
                                (error "TLB-FLUSH-PAGE expects an integer, label, or memory operand, got ~S."
                                       address)))))
    (encode-instruction state 'invlpg (list memory-operand))))

(defun emit-tlb-flush-all (environment)
  (require-bits environment 32 "TLB-FLUSH-ALL")
  (let ((state (compile-environment-state environment)))
    (encode-instruction state 'mov '(eax cr3))
    (encode-instruction state 'mov '(cr3 eax))))

(defun emit-load-tr (environment args)
  (require-bits environment 32 "LOAD-TR")
  (let* ((selector (resolve-operand environment
                                    (or (first args)
                                        (error "LOAD-TR requires a TSS selector."))))
         (state (compile-environment-state environment)))
    (cond ((integerp selector)
           (unless (typep selector '(integer 0 #xFFFF))
             (error "LOAD-TR selector must fit in 16 bits, got ~S." selector))
           (encode-instruction state 'mov (list 'ax selector))
           (encode-instruction state 'ltr '(ax)))
          ((or (word-register-p selector) (memory-operand-p selector))
           (encode-instruction state 'ltr (list selector)))
          (t
           (error "LOAD-TR expects an integer selector, 16-bit register, or memory operand, got ~S."
                  selector)))))

(defun emit-wrmsr-32 (state msr-index value)
  "Emit MOV ECX,msr; MOV EAX,low32(value); MOV EDX,0; WRMSR.
VALUE may be an integer or a label/linear reference (treated as 32-bit immediate)."
  (encode-instruction state 'mov (list 'ecx msr-index))
  (encode-instruction state 'mov (list 'eax value))
  (encode-instruction state 'mov (list 'edx 0))
  (encode-instruction state 'wrmsr '()))

(defun ensure-linear-immediate (value)
  "Promote a bare label symbol to a (:linear label) reference so the assembler
patches the linear address rather than the section-relative offset.  Integers
and explicit (:linear ...) / (:linear+ ...) forms pass through unchanged."
  (cond ((integerp value) value)
        ((and (symbolp value) (not (keywordp value))) (list :linear value))
        (t value)))

(defun emit-sysenter-setup (environment args)
  "Program IA32_SYSENTER_CS/ESP/EIP for the SYSENTER fast-call path.
SYSENTER hardcodes the new CS.Base to 0, so :HANDLER-LINEAR must be a true
linear address — labels are auto-promoted to (:linear label)."
  (require-bits environment 32 "SYSENTER-SETUP")
  (let* ((args (resolve-operands environment args))
         (state (compile-environment-state environment))
         (kernel-cs (or (getf args :kernel-cs)
                        (error "SYSENTER-SETUP requires :KERNEL-CS.")))
         (kernel-stack (ensure-linear-immediate
                        (or (getf args :kernel-stack)
                            (error "SYSENTER-SETUP requires :KERNEL-STACK."))))
         (handler (ensure-linear-immediate
                   (or (getf args :handler-linear)
                       (error "SYSENTER-SETUP requires :HANDLER-LINEAR.")))))
    (emit-wrmsr-32 state #x174 kernel-cs)
    (emit-wrmsr-32 state #x175 kernel-stack)
    (emit-wrmsr-32 state #x176 handler)))

(defun emit-syscall-setup (environment args)
  "Program IA32_STAR/LSTAR/FMASK and set EFER.SCE for the SYSCALL/SYSRET path.
The form emits valid WRMSR sequences on x86-32; SYSCALL execution itself
requires long mode (Phase 1: x86-64 backend)."
  (require-bits environment 32 "SYSCALL-SETUP")
  (let* ((args (resolve-operands environment args))
         (state (compile-environment-state environment))
         (kernel-cs (or (getf args :kernel-cs)
                        (error "SYSCALL-SETUP requires :KERNEL-CS.")))
         (user-cs (or (getf args :user-cs)
                      (error "SYSCALL-SETUP requires :USER-CS.")))
         (handler (or (getf args :handler-linear)
                      (error "SYSCALL-SETUP requires :HANDLER-LINEAR.")))
         (flags-mask (getf args :flags-mask 0)))
    (unless (and (integerp kernel-cs) (typep kernel-cs '(integer 0 #xFFFF)))
      (error "SYSCALL-SETUP :KERNEL-CS must be a 16-bit integer, got ~S." kernel-cs))
    (unless (and (integerp user-cs) (typep user-cs '(integer 0 #xFFFF)))
      (error "SYSCALL-SETUP :USER-CS must be a 16-bit integer, got ~S." user-cs))
    ;; IA32_STAR (0xC0000081): bits 47:32 = kernel-cs, bits 63:48 = user-cs.
    ;; On x86-32 we still write the low 32 (zero) and high 32 (the selectors).
    (let ((star-high (logior (ldb (byte 16 0) kernel-cs)
                             (ash (ldb (byte 16 0) user-cs) 16))))
      (encode-instruction state 'mov (list 'ecx #xC0000081))
      (encode-instruction state 'mov (list 'eax 0))
      (encode-instruction state 'mov (list 'edx star-high))
      (encode-instruction state 'wrmsr '()))
    ;; IA32_LSTAR (0xC0000082) = handler-linear (low 32; high 32 = 0 in our setup).
    (emit-wrmsr-32 state #xC0000082 (ensure-linear-immediate handler))
    ;; IA32_FMASK (0xC0000084) = flags-mask.
    (emit-wrmsr-32 state #xC0000084 flags-mask)
    ;; IA32_EFER (0xC0000080): set SCE (bit 0). Read-modify-write.
    (encode-instruction state 'mov (list 'ecx #xC0000080))
    (encode-instruction state 'rdmsr '())
    (encode-instruction state 'or (list 'eax 1))
    (encode-instruction state 'wrmsr '())))

(defun emit-privilege-drop (environment args)
  "Emit the IRET-based ring-0 → ring-3 transition.
Pushes SS3, ESP3, EFLAGS, CS3, EIP3 then IRETs."
  (require-bits environment 32 "PRIVILEGE-DROP")
  (let* ((args (resolve-operands environment args))
         (state (compile-environment-state environment))
         (data-selector (or (getf args :data-selector)
                            (error "PRIVILEGE-DROP requires :DATA-SELECTOR.")))
         (code-selector (or (getf args :code-selector)
                            (error "PRIVILEGE-DROP requires :CODE-SELECTOR.")))
         (stack-linear (or (getf args :stack-linear)
                           (error "PRIVILEGE-DROP requires :STACK-LINEAR.")))
         (entry-point (or (getf args :entry-point)
                          (error "PRIVILEGE-DROP requires :ENTRY-POINT.")))
         (eflags (getf args :eflags #x202)))
    (when (and (integerp data-selector)
               (not (typep data-selector '(integer 0 #xFFFF))))
      (error "PRIVILEGE-DROP :DATA-SELECTOR must fit in 16 bits, got ~S." data-selector))
    (when (and (integerp code-selector)
               (not (typep code-selector '(integer 0 #xFFFF))))
      (error "PRIVILEGE-DROP :CODE-SELECTOR must fit in 16 bits, got ~S." code-selector))
    (encode-instruction state 'push (list data-selector))
    (encode-instruction state 'push (list stack-linear))
    (encode-instruction state 'push (list eflags))
    (encode-instruction state 'push (list code-selector))
    (encode-instruction state 'push (list entry-point))
    (encode-instruction state 'iret '())))

(defparameter +tss-bytes+ 104)

(defun emit-tss (environment form)
  "Emit a 104-byte 32-bit available TSS labeled NAME.
Layout:
  0x04  ESP0  : :RING0-STACK-LINEAR    (linear address of ring-0 stack top)
  0x08  SS0   : :RING0-STACK-SELECTOR  (kernel data selector)
  0x66  IOPB  : 104 (effectively no I/O permission bitmap)
All other fields zero. Use with (:tss :label NAME) in a GDT and (load-tr ...)."
  (let* ((state (compile-environment-state environment))
         (name (or (second form)
                   (error "TSS requires a name.")))
         (raw-options (cddr form))
         (option-plist (if (and (consp (first raw-options))
                                (let ((k (ignore-errors (token-keyword (first (first raw-options))))))
                                  (and k (keywordp k))))
                           (first raw-options)
                           raw-options))
         (esp0 (or (getf option-plist :ring0-stack-linear)
                   (error "TSS ~S requires :RING0-STACK-LINEAR." name)))
         (ss0-form (or (getf option-plist :ring0-stack-selector)
                       (error "TSS ~S requires :RING0-STACK-SELECTOR." name)))
         (ss0 (resolve-operand environment ss0-form)))
    (unless (and (integerp ss0) (typep ss0 '(integer 0 #xFFFF)))
      (error "TSS :RING0-STACK-SELECTOR must be a 16-bit integer, got ~S." ss0-form))
    (emit-align state 4)
    (emit-label state name)
    ;; 0x00 LINK (16) + reserved (16)
    (emit-u32 state 0)
    ;; 0x04 ESP0
    (emit-imm32 state (resolve-operand environment esp0))
    ;; 0x08 SS0 (16) + reserved (16)
    (emit-u16 state ss0)
    (emit-u16 state 0)
    ;; 0x0C..0x65 zero — ESP1/SS1/ESP2/SS2/CR3/EIP/EFLAGS/regs/segs/LDT/T-flag
    (loop repeat (- #x66 #xC) do (emit-u8 state 0))
    ;; 0x66 IOPB offset = +tss-bytes+ (no IOPB)
    (emit-u16 state +tss-bytes+)))

(defun ensure-pic8259-controller (controller)
  (unless (typep controller 'pic8259-controller)
    (error "The current machine uses an unsupported interrupt controller ~S." controller))
  controller)

(defun emit-pic-remap-with-controller (environment controller args)
  (let* ((state (compile-environment-state environment))
         (args (resolve-operands environment args))
         (controller (ensure-pic8259-controller controller))
         (master-base (pic8259-controller-master-base controller))
         (slave-base (pic8259-controller-slave-base controller))
         (master-mask-port (pic8259-controller-master-mask-port controller))
         (slave-mask-port (pic8259-controller-slave-mask-port controller))
         (master-offset (getf args :vector-base
                              (getf args :master-offset
                                    (pic8259-controller-vector-base controller))))
         (slave-offset (getf args :slave-offset (+ master-offset 8)))
         (master-mask (getf args :master-mask #xFF))
         (slave-mask (getf args :slave-mask #xFF))
         (icw1 (getf args :icw1 #x11))
         (icw4 (getf args :icw4 #x01))
         (slave-identity (pic8259-controller-slave-identity controller)))
    (flet ((write-port (port value)
             (encode-instruction state 'mov (list 'al value))
             (encode-instruction state 'out (list port 'al))
             (emit-io-wait environment)))
      (write-port master-base icw1)
      (write-port slave-base icw1)
      (write-port master-mask-port master-offset)
      (write-port slave-mask-port slave-offset)
      (write-port master-mask-port (ash 1 slave-identity))
      (write-port slave-mask-port slave-identity)
      (write-port master-mask-port icw4)
      (write-port slave-mask-port icw4)
      (encode-instruction state 'mov (list 'al master-mask))
      (encode-instruction state 'out (list master-mask-port 'al))
      (encode-instruction state 'mov (list 'al slave-mask))
      (encode-instruction state 'out (list slave-mask-port 'al)))))

(defun emit-pic-remap (environment args)
  (emit-pic-remap-with-controller environment
                                  (or (machine-descriptor-interrupt-controller
                                       (environment-machine environment))
                                      (make-pic8259-controller))
                                  args))

(defun emit-pic-mask-all (environment args)
  (let* ((state (compile-environment-state environment))
         (args (resolve-operands environment args))
         (controller (ensure-pic8259-controller
                      (or (machine-descriptor-interrupt-controller
                           (environment-machine environment))
                          (make-pic8259-controller))))
         (master-mask (getf args :master #xFF))
         (slave-mask (getf args :slave #xFF)))
    (encode-instruction state 'mov (list 'al master-mask))
    (encode-instruction state 'out (list (pic8259-controller-master-mask-port controller) 'al))
    (encode-instruction state 'mov (list 'al slave-mask))
    (encode-instruction state 'out (list (pic8259-controller-slave-mask-port controller) 'al))))

(defun emit-pic-eoi (environment args)
  (let* ((state (compile-environment-state environment))
         (controller (ensure-pic8259-controller
                      (or (machine-descriptor-interrupt-controller
                           (environment-machine environment))
                          (make-pic8259-controller))))
         (irq (and args (resolve-operand environment (first args)))))
    (encode-instruction state 'mov '(al #x20))
    (when (and irq (>= irq 8))
      (encode-instruction state 'out (list (pic8259-controller-slave-base controller) 'al)))
    (encode-instruction state 'out (list (pic8259-controller-master-base controller) 'al))))

(defun emit-pic-mask-operation (environment irqs mask-p)
  (let* ((state (compile-environment-state environment))
         (controller (ensure-pic8259-controller
                      (or (machine-descriptor-interrupt-controller
                           (environment-machine environment))
                          (make-pic8259-controller)))))
    (unless irqs
      (error "~A requires at least one IRQ number." (if mask-p "PIC-MASK" "PIC-UNMASK")))
    (dolist (irq (resolve-operands environment irqs))
      (unless (typep irq '(integer 0 15))
        (error "IRQ number must be in the range 0..15, got ~S." irq))
      (let* ((port (if (< irq 8)
                       (pic8259-controller-master-mask-port controller)
                       (pic8259-controller-slave-mask-port controller)))
             (bit (ash 1 (if (< irq 8) irq (- irq 8))))
             (mask (if mask-p bit (logand #xFF (lognot bit)))))
        (encode-instruction state 'in (list 'al port))
        (encode-instruction state
                            (if mask-p 'or 'and)
                            (list 'al mask))
        (encode-instruction state 'out (list port 'al))))))

(defun pit-mode-value (designator)
  (etypecase designator
    (integer designator)
    (symbol
     (ecase (token-keyword designator)
       (:interrupt-on-terminal-count 0)
       (:one-shot 1)
       (:rate-generator 2)
       (:square-wave 3)
       (:software-strobe 4)
       (:hardware-strobe 5)))))

(defun pit-access-value (designator)
  (etypecase designator
    (integer designator)
    (symbol
     (ecase (token-keyword designator)
       (:lobyte 1)
       (:hibyte 2)
       (:low/high 3)
       (:lo/hi 3)
       (:word 3)))))

(defun emit-pit-set-frequency (environment args)
  (let* ((state (compile-environment-state environment))
         (args (resolve-operands environment args))
         (frequency (or (first args)
                        (error "PIT-SET-FREQUENCY requires a target frequency.")))
         (device (machine-timer-device-or-die environment))
         (channel (getf (rest args) :channel 0))
         (mode (pit-mode-value (getf (rest args) :mode :square-wave)))
         (access (pit-access-value (getf (rest args) :access :low/high)))
         (base-frequency (getf (rest args) :base-frequency
                               (pit8253-timer-base-frequency device))))
    (unless (typep frequency '(integer 1 *))
      (error "PIT-SET-FREQUENCY requires a positive integer frequency, got ~S." frequency))
    (unless (typep channel '(integer 0 2))
      (error "PIT channel must be 0, 1, or 2, got ~S." channel))
    (let* ((divisor (truncate base-frequency frequency))
           (clamped-divisor (max 1 (min 65535 divisor)))
           (channel-port (ecase channel
                           (0 (pit8253-timer-channel0-port device))
                           (1 (pit8253-timer-channel1-port device))
                           (2 (pit8253-timer-channel2-port device))))
           (control (logior (ash channel 6)
                            (ash access 4)
                            (ash mode 1))))
      (encode-instruction state 'mov (list 'al control))
      (encode-instruction state 'out (list (pit8253-timer-control-port device) 'al))
      (ecase access
        (1
         (encode-instruction state 'mov (list 'al (ldb (byte 8 0) clamped-divisor)))
         (encode-instruction state 'out (list channel-port 'al)))
        (2
         (encode-instruction state 'mov (list 'al (ldb (byte 8 8) clamped-divisor)))
         (encode-instruction state 'out (list channel-port 'al)))
        (3
         (encode-instruction state 'mov (list 'al (ldb (byte 8 0) clamped-divisor)))
         (encode-instruction state 'out (list channel-port 'al))
         (encode-instruction state 'mov (list 'al (ldb (byte 8 8) clamped-divisor)))
         (encode-instruction state 'out (list channel-port 'al)))))))

(defun emit-irq-remap (environment args)
  (emit-pic-remap-with-controller environment
                                  (machine-interrupt-controller-or-die environment)
                                  args))

(defun emit-irq-mask (environment args)
  (emit-pic-mask-operation environment args t))

(defun emit-irq-unmask (environment args)
  (emit-pic-mask-operation environment args nil))

(defun emit-irq-end-of-interrupt (environment args)
  (emit-pic-eoi environment args))

(defun emit-irq-handler (environment args)
  (let* ((state (compile-environment-state environment))
         (args (resolve-operands environment args))
         (irq (or (first args)
                  (error "IRQ-HANDLER requires an IRQ number.")))
         (handler (or (second args)
                      (error "IRQ-HANDLER requires a handler label."))))
    (unless (and (integerp irq) (<= 0 irq 255))
      (error "IRQ-HANDLER IRQ must be an integer from 0 to 255, got ~S." irq))
    (encode-instruction state 'call (list handler))
    (emit-irq-end-of-interrupt environment (list irq))
    (encode-instruction state 'iret '())))

(defun emit-timer-set-frequency (environment args)
  (typecase (machine-timer-device-or-die environment)
    (pit8253-timer
     (emit-pit-set-frequency environment args))
    (t
     (error "The current machine uses an unsupported timer device."))))

(defun emit-timer-enable (environment)
  (let ((device (machine-timer-device-or-die environment)))
    (typecase device
      (pit8253-timer
       (emit-irq-unmask environment (list (pit8253-timer-irq device))))
      (t
       (error "The current machine uses an unsupported timer device.")))))

(defun emit-timer-disable (environment)
  (let ((device (machine-timer-device-or-die environment)))
    (typecase device
      (pit8253-timer
       (emit-irq-mask environment (list (pit8253-timer-irq device))))
      (t
       (error "The current machine uses an unsupported timer device.")))))

(defun emit-pit-read-counter (environment args)
  (let* ((state (compile-environment-state environment))
         (args (resolve-operands environment args))
         (destination (or (first args)
                          (error "TIMER-READ-COUNTER requires a destination register.")))
         (device (machine-timer-device-or-die environment))
         (channel (getf (rest args) :channel 0)))
    (unless (typep channel '(integer 0 2))
      (error "PIT channel must be 0, 1, or 2, got ~S." channel))
    (unless (or (word-register-p destination)
                (dword-register-p destination))
      (error "TIMER-READ-COUNTER requires a 16-bit or 32-bit destination register, got ~S."
             destination))
    (let ((channel-port (ecase channel
                          (0 (pit8253-timer-channel0-port device))
                          (1 (pit8253-timer-channel1-port device))
                          (2 (pit8253-timer-channel2-port device))))
          (latch-command (ash channel 6)))
      (encode-instruction state 'mov (list 'al latch-command))
      (encode-instruction state 'out (list (pit8253-timer-control-port device) 'al))
      (encode-instruction state 'mov (list 'dx channel-port))
      (encode-instruction state 'in '(al dx))
      (encode-instruction state 'mov '(ah al))
      (encode-instruction state 'in '(al dx))
      (encode-instruction state 'xchg '(al ah))
      (cond ((eql (token-keyword destination) :AX)
             nil)
            ((word-register-p destination)
             (encode-instruction state 'mov (list destination 'ax)))
            (t
             (encode-instruction state 'movzx (list destination 'ax)))))))

(defun emit-timer-read-counter (environment args)
  (typecase (machine-timer-device-or-die environment)
    (pit8253-timer
     (emit-pit-read-counter environment args))
    (t
     (error "The current machine uses an unsupported timer device."))))

(defun emit-uart-init (environment args)
  (let* ((state (compile-environment-state environment))
         (args (resolve-operands environment args))
         (device (resolve-serial-device environment (first args)))
         (base (uart16550-device-base-port device))
         (divisor (uart16550-device-divisor device)))
    (dolist (port/value `((,(+ base 3) #x80)
                          (,base ,(ldb (byte 8 0) divisor))
                          (,(+ base 1) ,(ldb (byte 8 8) divisor))
                          (,(+ base 3) #x03)
                          (,(+ base 2) #xC7)
                          (,(+ base 4) #x0B)))
      (destructuring-bind (port value) port/value
        (encode-instruction state 'mov (list 'dx port))
        (encode-instruction state 'mov (list 'al value))
        (encode-instruction state 'out '(dx al))))))

(defun emit-uart-write-byte (environment args)
  (let* ((state (compile-environment-state environment))
         (args (resolve-operands environment args))
         (device (resolve-serial-device environment (first args)))
         (source (or (second args)
                     (error "UART-WRITE-BYTE requires a device and source register.")))
         (wait-label (gensym "UART-TX-WAIT-"))
         (base (uart16550-device-base-port device))
         (status-port (+ base 5))
         (data-port base))
    (unless (byte-register-p source)
      (error "UART-WRITE-BYTE currently requires a byte register source, got ~S." source))
    (when (eql (token-keyword source) :AL)
      (if (= (assembly-bits state) 16)
          (encode-instruction state 'push '(ax))
          (encode-instruction state 'push '(eax))))
    (emit-label state wait-label)
    (encode-instruction state 'mov (list 'dx status-port))
    (encode-instruction state 'in '(al dx))
    (encode-instruction state 'and '(al #x20))
    (encode-instruction state 'jz (list wait-label))
    (cond ((eql (token-keyword source) :AL)
           (if (= (assembly-bits state) 16)
               (encode-instruction state 'pop '(ax))
               (encode-instruction state 'pop '(eax))))
          (t
           (encode-instruction state 'mov (list 'al source))))
    (encode-instruction state 'mov (list 'dx data-port))
    (encode-instruction state 'out '(dx al))))

(defun emit-uart-read-byte (environment args)
  (let* ((state (compile-environment-state environment))
         (args (resolve-operands environment args))
         (device (resolve-serial-device environment (first args)))
         (destination (or (second args)
                          (error "UART-READ-BYTE requires a device and destination register.")))
         (wait-label (gensym "UART-RX-WAIT-"))
         (base (uart16550-device-base-port device))
         (status-port (+ base 5))
         (data-port base))
    (unless (byte-register-p destination)
      (error "UART-READ-BYTE currently requires a byte register destination, got ~S."
             destination))
    (emit-label state wait-label)
    (encode-instruction state 'mov (list 'dx status-port))
    (encode-instruction state 'in '(al dx))
    (encode-instruction state 'and '(al #x01))
    (encode-instruction state 'jz (list wait-label))
    (encode-instruction state 'mov (list 'dx data-port))
    (encode-instruction state 'in '(al dx))
    (unless (eql (token-keyword destination) :AL)
      (encode-instruction state 'mov (list destination 'al)))))

(defun emit-uart-tx-ready-p (environment args)
  (let* ((state (compile-environment-state environment))
         (args (resolve-operands environment args))
         (device (resolve-serial-device environment (first args)))
         (ready-label (or (second args)
                          (error "UART-TX-READY-P requires a device and branch label.")))
         (status-port (+ (uart16550-device-base-port device) 5)))
    (encode-instruction state 'mov (list 'dx status-port))
    (encode-instruction state 'in '(al dx))
    (encode-instruction state 'and '(al #x20))
    (encode-instruction state 'jnz (list ready-label))))

(defun emit-uart-print (environment args)
  (let* ((state (compile-environment-state environment))
         (args (resolve-operands environment args))
         (device (resolve-serial-device environment (first args)))
         (string-designator (or (second args)
                                (error "UART-PRINT requires a device and string designator.")))
         (skip-label (gensym "AFTER-UART-STRING-"))
         (string-label (and (stringp string-designator)
                            (gensym "UART-STRING-")))
         (loop-label (gensym "UART-PRINT-LOOP-"))
         (wait-label (gensym "UART-TX-READY-"))
         (done-label (gensym "UART-PRINT-DONE-"))
         (string-register (helper-string-register environment))
         (base (uart16550-device-base-port device))
         (status-port (+ base 5))
         (data-port base))
    (when string-label
      (encode-instruction state 'jmp (list skip-label))
      (emit-label state string-label)
      (emit-ascii-z state string-designator)
      (emit-label state skip-label))
    (encode-instruction state 'mov (list string-register
                                        (or string-label
                                            (print-label-designator string-designator))))
    (encode-instruction state 'cld '())
    (emit-label state loop-label)
    (encode-instruction state 'lodsb '())
    (encode-instruction state 'or '(al al))
    (encode-instruction state 'jz (list done-label))
    (encode-instruction state 'mov '(ah al))
    (emit-label state wait-label)
    (encode-instruction state 'mov (list 'dx status-port))
    (encode-instruction state 'in '(al dx))
    (encode-instruction state 'and '(al #x20))
    (encode-instruction state 'jz (list wait-label))
    (encode-instruction state 'mov '(al ah))
    (encode-instruction state 'mov (list 'dx data-port))
    (encode-instruction state 'out '(dx al))
    (encode-instruction state 'jmp (list loop-label))
    (emit-label state done-label)))

(defun emit-vga-print-hex (environment value-designator args)
  (require-bits environment 32 "VGA-PRINT-HEX")
  (let* ((state (compile-environment-state environment))
         (args (resolve-operands environment args))
         (value (resolve-operand environment value-designator))
         (attribute (getf args :attribute #x1F))
         (address (resolve-vga-address args))
         (section-base (section-linear-base (compile-environment-section environment)))
         (video-offset (- address section-base)))
    (when (minusp video-offset)
      (error "VGA address #x~X is below the current section base #x~X." address section-base))
    (cond ((integerp value)
           (emit-vga-print environment
                           (format nil "~8,'0X" (ldb (byte 32 0) value))
                           (list :address address :attribute attribute)))
          ((dword-register-p value)
           (mark-helper-needed environment :vga-print-hex8)
           (encode-instruction state 'mov (list 'ebx value))
           (encode-instruction state 'mov (list 'edi video-offset))
           (encode-instruction state 'mov (list 'ah attribute))
           (encode-instruction state 'call (list '__bootwright-vga-print-hex8)))
          (t
           (error "VGA-PRINT-HEX expects a 32-bit register or integer, got ~S."
                  value-designator)))))

(defun emit-vga-print-decimal (environment value-designator args)
  (require-bits environment 32 "VGA-PRINT-DECIMAL")
  (let* ((state (compile-environment-state environment))
         (args (resolve-operands environment args))
         (value (resolve-operand environment value-designator))
         (attribute (getf args :attribute #x1F))
         (address (resolve-vga-address args))
         (section-base (section-linear-base (compile-environment-section environment)))
         (video-offset (- address section-base)))
    (when (minusp video-offset)
      (error "VGA address #x~X is below the current section base #x~X." address section-base))
    (cond ((integerp value)
           (emit-vga-print environment
                           (format nil "~10,'0D" (ldb (byte 32 0) value))
                           (list :address address :attribute attribute)))
          ((dword-register-p value)
           (mark-helper-needed environment :vga-print-decimal)
           (encode-instruction state 'mov (list 'ebx value))
           (encode-instruction state 'mov (list 'edi video-offset))
           (encode-instruction state 'mov (list 'ah attribute))
           (encode-instruction state 'call (list '__bootwright-vga-print-decimal)))
          (t
           (error "VGA-PRINT-DECIMAL expects a 32-bit register or integer, got ~S."
                  value-designator)))))

(defun emit-vga-print-ascii4 (environment value-designator args)
  (require-bits environment 32 "VGA-PRINT-ASCII4")
  (let* ((state (compile-environment-state environment))
         (args (resolve-operands environment args))
         (value (resolve-operand environment value-designator))
         (attribute (getf args :attribute #x1F))
         (address (resolve-vga-address args))
         (section-base (section-linear-base (compile-environment-section environment)))
         (video-offset (- address section-base)))
    (when (minusp video-offset)
      (error "VGA address #x~X is below the current section base #x~X." address section-base))
    (unless (dword-register-p value)
      (error "VGA-PRINT-ASCII4 expects a 32-bit register, got ~S." value-designator))
    (mark-helper-needed environment :vga-print-ascii4)
    (encode-instruction state 'mov (list 'ebx value))
    (encode-instruction state 'mov (list 'edi video-offset))
    (encode-instruction state 'mov (list 'ah attribute))
    (encode-instruction state 'call (list '__bootwright-vga-print-ascii4))))

(defun emit-vga-set-cursor (environment args)
  (let* ((state (compile-environment-state environment))
         (args (resolve-operands environment args))
         (row (getf args :row 0))
         (column (getf args :column 0))
         (columns (getf args :columns 80))
         (position (+ column (* row columns))))
    (unless (and (integerp row) (integerp column))
      (error "VGA-SET-CURSOR currently requires integer :ROW and :COLUMN, got ~S/~S."
             row column))
    (encode-instruction state 'mov '(dx #x3D4))
    (encode-instruction state 'mov '(al #x0F))
    (encode-instruction state 'out '(dx al))
    (encode-instruction state 'mov '(dx #x3D5))
    (encode-instruction state 'mov (list 'al (ldb (byte 8 0) position)))
    (encode-instruction state 'out '(dx al))
    (encode-instruction state 'mov '(dx #x3D4))
    (encode-instruction state 'mov '(al #x0E))
    (encode-instruction state 'out '(dx al))
    (encode-instruction state 'mov '(dx #x3D5))
    (encode-instruction state 'mov (list 'al (ldb (byte 8 8) position)))
    (encode-instruction state 'out '(dx al))))

(defun emit-serial-init (environment args)
  (let* ((state (compile-environment-state environment))
         (args (resolve-operands environment args))
         (baud (getf args :baud 38400))
         (base-clock (getf args :base-clock 115200))
         (lcr (getf args :lcr #x03))
         (fcr (getf args :fcr #xC7))
         (mcr (getf args :mcr #x0B)))
    (unless (and (integerp baud) (plusp baud))
      (error "SERIAL-INIT requires a positive :BAUD, got ~S." baud))
    (let* ((divisor (max 1 (min 65535 (truncate base-clock baud))))
           (div-lo (ldb (byte 8 0) divisor))
           (div-hi (ldb (byte 8 8) divisor)))
      (flet ((write-port (port value)
               (encode-instruction state 'mov (list 'dx port))
               (encode-instruction state 'mov (list 'al value))
               (encode-instruction state 'out '(dx al))))
        (write-port #x3F9 #x00)
        (write-port #x3FB #x80)
        (write-port #x3F8 div-lo)
        (write-port #x3F9 div-hi)
        (write-port #x3FB lcr)
        (write-port #x3FA fcr)
        (write-port #x3FC mcr)))))

(defun emit-serial-print (environment string-designator)
  (let* ((state (compile-environment-state environment))
         (skip-label (gensym "AFTER-SERIAL-STRING-"))
         (string-label (and (stringp string-designator)
                            (gensym "SERIAL-STRING-")))
         (string-register (helper-string-register environment)))
    (mark-helper-needed environment :serial-print-z)
    (when string-label
      (encode-instruction state 'jmp (list skip-label))
      (emit-label state string-label)
      (emit-ascii-z state string-designator)
      (emit-label state skip-label))
    (encode-instruction state 'mov (list string-register
                                         (or string-label
                                             (print-label-designator string-designator))))
    (encode-instruction state 'call (list '__bootwright-serial-print-z))))

(defun dword-immediate-designator-p (operand)
  (let ((normalized (normalize-immediate operand)))
    (or (integerp normalized)
        (label-reference-p normalized)
        (linear-reference-p normalized)
        (linear-addend-reference-p normalized))))

(defun same-register-designator-p (left right)
  (and (typep left '(or symbol string))
       (typep right '(or symbol string))
       (eql (token-keyword left) (token-keyword right))))

(defun normalize-memory-unit (designator form-name)
  (case (token-keyword (or designator :byte))
    (:byte :byte)
    (:word :word)
    (:dword :dword)
    (t
     (error "~A :UNIT must be one of :BYTE, :WORD, or :DWORD, got ~S."
            form-name designator))))

(defun unit-copy-operator (unit)
  (ecase unit
    (:byte 'movsb)
    (:word 'movsw)
    (:dword 'movsd)))

(defun unit-fill-operator (unit)
  (ecase unit
    (:byte 'stosb)
    (:word 'stosw)
    (:dword 'stosd)))

(defun unit-compare-operator (unit)
  (ecase unit
    (:byte 'cmpsb)
    (:word 'cmpsw)
    (:dword 'cmpsd)))

(defun emit-load-pointer-register-32 (state destination designator form-name)
  (cond ((memory-operand-p designator)
         (error "~A expects a pointer register or address designator, not a memory dereference ~S."
                form-name designator))
        ((dword-register-p designator)
         (unless (same-register-designator-p destination designator)
           (encode-instruction state 'mov (list destination designator))))
        ((dword-immediate-designator-p designator)
         (encode-instruction state 'mov (list destination designator)))
        (t
         (error "~A expects a 32-bit pointer register or address designator, got ~S."
                form-name designator))))

(defun emit-load-count-register-32 (state designator form-name)
  (cond ((dword-register-p designator)
         (unless (same-register-designator-p 'ecx designator)
           (encode-instruction state 'mov (list 'ecx designator))))
        ((integerp designator)
         (encode-instruction state 'mov (list 'ecx designator)))
        (t
         (error "~A expects COUNT to be an integer or 32-bit register, got ~S."
                form-name designator))))

(defun emit-load-fill-value (state unit designator form-name)
  (ecase unit
    (:byte
     (cond ((byte-register-p designator)
            (unless (same-register-designator-p 'al designator)
              (encode-instruction state 'mov (list 'al designator))))
           ((integerp designator)
            (encode-instruction state 'mov (list 'al designator)))
           (t
            (error "~A with :UNIT :BYTE expects an integer or byte register value, got ~S."
                   form-name designator))))
    (:word
     (cond ((word-register-p designator)
            (unless (same-register-designator-p 'ax designator)
              (encode-instruction state 'mov (list 'ax designator))))
           ((integerp designator)
            (encode-instruction state 'mov (list 'ax designator)))
           (t
            (error "~A with :UNIT :WORD expects an integer or word register value, got ~S."
                   form-name designator))))
    (:dword
     (cond ((dword-register-p designator)
            (unless (same-register-designator-p 'eax designator)
              (encode-instruction state 'mov (list 'eax designator))))
           ((integerp designator)
            (encode-instruction state 'mov (list 'eax designator)))
           (t
            (error "~A with :UNIT :DWORD expects an integer or 32-bit register value, got ~S."
                   form-name designator))))))

(defun emit-copy-memory (environment args)
  (require-bits environment 32 "COPY-MEMORY")
  (unless (>= (length args) 3)
    (error "COPY-MEMORY expects DESTINATION SOURCE COUNT [&key :UNIT], got ~S." args))
  (let* ((state (compile-environment-state environment))
         (args (resolve-operands environment args))
         (destination (first args))
         (source (second args))
         (count (third args))
         (options (nthcdr 3 args))
         (unit (normalize-memory-unit (getf options :unit :byte) "COPY-MEMORY")))
    (emit-load-pointer-register-32 state 'edi destination "COPY-MEMORY")
    (emit-load-pointer-register-32 state 'esi source "COPY-MEMORY")
    (emit-load-count-register-32 state count "COPY-MEMORY")
    (encode-instruction state 'cld '())
    (encode-instruction state 'rep (list (unit-copy-operator unit)))))

(defun emit-fill-memory (environment args)
  (require-bits environment 32 "FILL-MEMORY")
  (unless (>= (length args) 3)
    (error "FILL-MEMORY expects DESTINATION VALUE COUNT [&key :UNIT], got ~S." args))
  (let* ((state (compile-environment-state environment))
         (args (resolve-operands environment args))
         (destination (first args))
         (value (second args))
         (count (third args))
         (options (nthcdr 3 args))
         (unit (normalize-memory-unit (getf options :unit :byte) "FILL-MEMORY")))
    (emit-load-pointer-register-32 state 'edi destination "FILL-MEMORY")
    (emit-load-fill-value state unit value "FILL-MEMORY")
    (emit-load-count-register-32 state count "FILL-MEMORY")
    (encode-instruction state 'cld '())
    (encode-instruction state 'rep (list (unit-fill-operator unit)))))

(defun emit-zero-memory (environment args)
  (require-bits environment 32 "ZERO-MEMORY")
  (unless (>= (length args) 2)
    (error "ZERO-MEMORY expects DESTINATION COUNT [&key :UNIT], got ~S." args))
  (let* ((state (compile-environment-state environment))
         (args (resolve-operands environment args))
         (destination (first args))
         (count (second args))
         (options (nthcdr 2 args))
         (unit (normalize-memory-unit (getf options :unit :byte) "ZERO-MEMORY")))
    (emit-load-pointer-register-32 state 'edi destination "ZERO-MEMORY")
    (encode-instruction state 'xor '(eax eax))
    (emit-load-count-register-32 state count "ZERO-MEMORY")
    (encode-instruction state 'cld '())
    (encode-instruction state 'rep (list (unit-fill-operator unit)))))

(defun emit-compare-memory (environment args)
  (require-bits environment 32 "COMPARE-MEMORY")
  (unless (>= (length args) 3)
    (error "COMPARE-MEMORY expects LEFT RIGHT COUNT [&key :UNIT :EQUAL :NOT-EQUAL], got ~S."
           args))
  (let* ((state (compile-environment-state environment))
         (args (resolve-operands environment args))
         (left (first args))
         (right (second args))
         (count (third args))
         (options (nthcdr 3 args))
         (unit (normalize-memory-unit (getf options :unit :byte) "COMPARE-MEMORY"))
         (equal-label (getf options :equal))
         (not-equal-label (getf options :not-equal))
         (after-compare (gensym "CMP-MEM-AFTER-")))
    (emit-load-pointer-register-32 state 'esi left "COMPARE-MEMORY")
    (emit-load-pointer-register-32 state 'edi right "COMPARE-MEMORY")
    (emit-load-count-register-32 state count "COMPARE-MEMORY")
    (encode-instruction state 'cld '())
    (encode-instruction state 'test '(ecx ecx))
    (encode-instruction state 'jz-near (list after-compare))
    (encode-instruction state 'repe (list (unit-compare-operator unit)))
    (emit-label state after-compare)
    (when equal-label
      (encode-instruction state 'je-near (list equal-label)))
    (when not-equal-label
      (encode-instruction state 'jne-near (list not-equal-label)))))

(defun emit-copy-string (environment args)
  (require-bits environment 32 "COPY-STRING")
  (unless (>= (length args) 2)
    (error "COPY-STRING expects DESTINATION SOURCE, got ~S." args))
  (let* ((state (compile-environment-state environment))
         (args (resolve-operands environment args))
         (destination (first args))
         (source (second args))
         (copy-loop (gensym "COPY-STRING-LOOP-")))
    (emit-load-pointer-register-32 state 'edi destination "COPY-STRING")
    (emit-load-pointer-register-32 state 'esi source "COPY-STRING")
    (encode-instruction state 'cld '())
    (emit-label state copy-loop)
    (encode-instruction state 'lodsb '())
    (encode-instruction state 'stosb '())
    (encode-instruction state 'test '(al al))
    (encode-instruction state 'jnz-near (list copy-loop))))

(defun emit-string-length (environment args)
  (require-bits environment 32 "STRING-LENGTH")
  (unless (>= (length args) 2)
    (error "STRING-LENGTH expects SOURCE DESTINATION-REGISTER, got ~S." args))
  (let* ((state (compile-environment-state environment))
         (args (resolve-operands environment args))
         (source (first args))
         (destination (second args)))
    (unless (dword-register-p destination)
      (error "STRING-LENGTH destination must be a 32-bit register, got ~S." destination))
    (emit-load-pointer-register-32 state 'edi source "STRING-LENGTH")
    (encode-instruction state 'mov '(ecx #xFFFFFFFF))
    (encode-instruction state 'xor '(eax eax))
    (encode-instruction state 'cld '())
    (encode-instruction state 'repne '(scasb))
    (encode-instruction state 'not '(ecx))
    (encode-instruction state 'dec '(ecx))
    (unless (same-register-designator-p destination 'ecx)
      (encode-instruction state 'mov (list destination 'ecx)))))

(defun emit-string-equal (environment args)
  (require-bits environment 32 "STRING-EQUAL")
  (unless (>= (length args) 2)
    (error "STRING-EQUAL expects LEFT RIGHT [&key :EQUAL :NOT-EQUAL], got ~S." args))
  (let* ((state (compile-environment-state environment))
         (args (resolve-operands environment args))
         (left (first args))
         (right (second args))
         (options (nthcdr 2 args))
         (equal-label (getf options :equal))
         (not-equal-label (getf options :not-equal))
         (compare-loop (gensym "STRING-EQUAL-LOOP-"))
         (strings-equal (gensym "STRING-EQUAL-YES-"))
         (dispatch (gensym "STRING-EQUAL-DISPATCH-"))
         (mismatch (gensym "STRING-EQUAL-NO-")))
    (emit-load-pointer-register-32 state 'esi left "STRING-EQUAL")
    (emit-load-pointer-register-32 state 'edi right "STRING-EQUAL")
    (encode-instruction state 'cld '())
    (emit-label state compare-loop)
    (encode-instruction state 'mov '(al (:mem :byte esi)))
    (encode-instruction state 'mov '(dl (:mem :byte edi)))
    (encode-instruction state 'cmp '(al dl))
    (encode-instruction state 'jne-near (list mismatch))
    (encode-instruction state 'test '(al al))
    (encode-instruction state 'jz-near (list strings-equal))
    (encode-instruction state 'inc '(esi))
    (encode-instruction state 'inc '(edi))
    (encode-instruction state 'jmp (list compare-loop))
    (emit-label state mismatch)
    (encode-instruction state 'jmp (list dispatch))
    (emit-label state strings-equal)
    (encode-instruction state 'test '(al al))
    (emit-label state dispatch)
    (when equal-label
      (encode-instruction state 'je-near (list equal-label)))
    (when not-equal-label
      (encode-instruction state 'jne-near (list not-equal-label)))))

(defun emit-pc-speaker-tone (environment args)
  (let* ((state (compile-environment-state environment))
         (args (resolve-operands environment args))
         (frequency (or (first args)
                        (error "PC-SPEAKER-TONE requires a positive frequency.")))
         (base-frequency (getf (rest args) :base-frequency 1193182)))
    (unless (and (integerp frequency) (plusp frequency))
      (error "PC-SPEAKER-TONE requires a positive integer frequency, got ~S." frequency))
    (let* ((divisor (max 1 (min 65535 (truncate base-frequency frequency))))
           (div-lo (ldb (byte 8 0) divisor))
           (div-hi (ldb (byte 8 8) divisor)))
      (encode-instruction state 'mov '(al #xB6))
      (encode-instruction state 'out '(#x43 al))
      (encode-instruction state 'mov (list 'al div-lo))
      (encode-instruction state 'out '(#x42 al))
      (encode-instruction state 'mov (list 'al div-hi))
      (encode-instruction state 'out '(#x42 al))
      (encode-instruction state 'in '(al #x61))
      (encode-instruction state 'or '(al #x03))
      (encode-instruction state 'out '(#x61 al)))))

(defun emit-pc-speaker-off (environment)
  (let ((state (compile-environment-state environment)))
    (encode-instruction state 'in '(al #x61))
    (encode-instruction state 'and '(al #xFC))
    (encode-instruction state 'out '(#x61 al))))

(defstruct (keyboard-descriptor
            (:constructor make-keyboard-descriptor
                (&key name controller irq buffer-size scancode-map
                      handler-label table-label shifted-table-label
                      buffer-label head-label tail-label shift-state-label)))
  name controller irq buffer-size scancode-map
  handler-label table-label shifted-table-label
  buffer-label head-label tail-label shift-state-label)

(defparameter +keyboard-us-qwerty-table+
  ;; 128-byte set-1 scancode -> ASCII (0 = unmapped / modifier / release-class).
  (let ((table (make-array 128 :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (aref table #x01) #x1B)                          ; ESC
    (loop for i from #x02 to #x0A
          do (setf (aref table i) (+ #x30 (- i 1))))       ; '1'..'9'
    (setf (aref table #x0B) #x30                           ; '0'
          (aref table #x0C) #x2D                           ; '-'
          (aref table #x0D) #x3D                           ; '='
          (aref table #x0E) #x08                           ; BACKSPACE
          (aref table #x0F) #x09)                          ; TAB
    (loop for ch across "qwertyuiop[]"
          for i from 0
          do (setf (aref table (+ #x10 i)) (char-code ch)))
    (setf (aref table #x1C) #x0A)                          ; ENTER -> LF
    (loop for ch across "asdfghjkl;'`"
          for i from 0
          do (setf (aref table (+ #x1E i)) (char-code ch)))
    (setf (aref table #x2B) #x5C)                          ; backslash
    (loop for ch across "zxcvbnm,./"
          for i from 0
          do (setf (aref table (+ #x2C i)) (char-code ch)))
    (setf (aref table #x37) #x2A                           ; numpad '*'
          (aref table #x39) #x20)                          ; SPACE
    table))

(defun clone-keyboard-table (table)
  (let ((copy (make-array (length table)
                          :element-type '(unsigned-byte 8)
                          :initial-element 0)))
    (replace copy table)
    copy))

(defparameter +keyboard-us-qwerty-shift-table+
  (let ((table (clone-keyboard-table +keyboard-us-qwerty-table+)))
    (loop for ch across "!@#$%^&*()"
          for i from #x02
          do (setf (aref table i) (char-code ch)))
    (setf (aref table #x0C) #x5F                           ; '_'
          (aref table #x0D) #x2B)                          ; '+'
    (loop for ch across "QWERTYUIOP{}"
          for i from 0
          do (setf (aref table (+ #x10 i)) (char-code ch)))
    (loop for ch across "ASDFGHJKL:\"~"
          for i from 0
          do (setf (aref table (+ #x1E i)) (char-code ch)))
    (setf (aref table #x2B) #x7C)                          ; '|'
    (loop for ch across "ZXCVBNM<>?"
          for i from 0
          do (setf (aref table (+ #x2C i)) (char-code ch)))
    table))

(defparameter +keyboard-lenovo-ideapad-3-table+
  (let ((table (clone-keyboard-table +keyboard-us-qwerty-table+)))
    ;; Treat the extra ISO/OEM key as backslash on Lenovo IdeaPad 3 style layouts.
    (setf (aref table #x56) #x5C)
    table))

(defparameter +keyboard-lenovo-ideapad-3-shift-table+
  (let ((table (clone-keyboard-table +keyboard-us-qwerty-shift-table+)))
    (setf (aref table #x56) #x7C)
    table))

(defun keyboard-layout-tables (layout)
  (ecase layout
    (:us-qwerty
     (values +keyboard-us-qwerty-table+
             +keyboard-us-qwerty-shift-table+))
    (:lenovo-ideapad-3
     (values +keyboard-lenovo-ideapad-3-table+
             +keyboard-lenovo-ideapad-3-shift-table+))))

(defun derive-keyboard-label (name suffix)
  (intern (concatenate 'string (symbol-name name) suffix)
          (or (symbol-package name) *package*)))

(defun lookup-keyboard (environment name)
  (or (gethash name (compile-environment-keyboards environment))
      (error "Unknown keyboard driver ~S — is (keyboard-driver ~S ...) declared in this section?"
             name name)))

(defun emit-keyboard-handler (environment kbd)
  (let* ((state (compile-environment-state environment))
         (shift-press-label (gensym "KBD-SHIFT-PRESS-"))
         (shift-release-label (gensym "KBD-SHIFT-RELEASE-"))
         (normal-table-label (gensym "KBD-NORMAL-TABLE-"))
         (eoi-label (gensym "KBD-EOI-"))
         (handler (keyboard-descriptor-handler-label kbd))
         (table (keyboard-descriptor-table-label kbd))
         (shifted-table (keyboard-descriptor-shifted-table-label kbd))
         (buffer (keyboard-descriptor-buffer-label kbd))
         (tail (keyboard-descriptor-tail-label kbd))
         (shift-state (keyboard-descriptor-shift-state-label kbd))
         (mask (1- (keyboard-descriptor-buffer-size kbd)))
         (irq (keyboard-descriptor-irq kbd)))
    (emit-label state handler)
    (encode-instruction state 'push '(eax))
    (encode-instruction state 'push '(ebx))
    (encode-instruction state 'push '(ecx))
    (encode-instruction state 'in (list 'al #x60))
    (encode-instruction state 'cmp '(al #x2A))
    (encode-instruction state 'je-near (list shift-press-label))
    (encode-instruction state 'cmp '(al #x36))
    (encode-instruction state 'je-near (list shift-press-label))
    (encode-instruction state 'cmp '(al #xAA))
    (encode-instruction state 'je-near (list shift-release-label))
    (encode-instruction state 'cmp '(al #xB6))
    (encode-instruction state 'je-near (list shift-release-label))
    (encode-instruction state 'test '(al #x80))
    (encode-instruction state 'jnz-near (list eoi-label))
    (encode-instruction state 'movzx '(ecx al))
    (encode-instruction state 'mov (list 'ebx table))
    (encode-instruction state 'cmp (list (list :MEM :BYTE shift-state) 0))
    (encode-instruction state 'je-near (list normal-table-label))
    (encode-instruction state 'mov (list 'ebx shifted-table))
    (emit-label state normal-table-label)
    (encode-instruction state 'mov (list 'bl (list :MEM :BYTE 'ebx 'ecx 1 0)))
    (encode-instruction state 'test '(bl bl))
    (encode-instruction state 'jz-near (list eoi-label))
    (encode-instruction state 'movzx (list 'ecx (list :MEM :BYTE tail)))
    (encode-instruction state 'mov (list 'eax buffer))
    (encode-instruction state 'mov (list (list :MEM :BYTE 'eax 'ecx 1 0) 'bl))
    (encode-instruction state 'inc '(cl))
    (encode-instruction state 'and (list 'cl mask))
    (encode-instruction state 'mov (list (list :MEM :BYTE tail) 'cl))
    (encode-instruction state 'jmp (list eoi-label))
    (emit-label state shift-press-label)
    (encode-instruction state 'mov (list (list :MEM :BYTE shift-state) 1))
    (encode-instruction state 'jmp (list eoi-label))
    (emit-label state shift-release-label)
    (encode-instruction state 'mov (list (list :MEM :BYTE shift-state) 0))
    (emit-label state eoi-label)
    (emit-irq-end-of-interrupt environment (list irq))
    (encode-instruction state 'pop '(ecx))
    (encode-instruction state 'pop '(ebx))
    (encode-instruction state 'pop '(eax))
    (encode-instruction state 'iret '())))

(defun parse-keyboard-driver-form (environment form)
  (let* ((name (or (second form)
                   (error "KEYBOARD-DRIVER requires a name.")))
         (options (resolve-operands environment (third form)))
         (controller (or (getf options :controller) :ps2))
         (irq (getf options :irq 1))
         (buffer-size (getf options :buffer-size 64))
         (scancode-map (or (getf options :scancode-map) :lenovo-ideapad-3)))
    (unless (eq controller :ps2)
      (error "KEYBOARD-DRIVER currently only supports :PS2, got ~S." controller))
    (unless (member scancode-map '(:us-qwerty :lenovo-ideapad-3))
      (error "KEYBOARD-DRIVER currently supports :US-QWERTY and :LENOVO-IDEAPAD-3, got ~S."
             scancode-map))
    (unless (and (integerp buffer-size)
                 (>= buffer-size 2)
                 (<= buffer-size 256)
                 (zerop (logand buffer-size (1- buffer-size))))
      (error "KEYBOARD-DRIVER :BUFFER-SIZE must be a power of two in 2..256, got ~S." buffer-size))
    (unless (typep irq '(integer 0 15))
      (error "KEYBOARD-DRIVER :IRQ must be in 0..15, got ~S." irq))
    (make-keyboard-descriptor
     :name name
     :controller controller
     :irq irq
     :buffer-size buffer-size
     :scancode-map scancode-map
     :handler-label (derive-keyboard-label name "-HANDLER")
     :table-label   (derive-keyboard-label name "-TABLE")
     :shifted-table-label (derive-keyboard-label name "-SHIFT-TABLE")
     :buffer-label  (derive-keyboard-label name "-BUFFER")
     :head-label    (derive-keyboard-label name "-HEAD")
     :tail-label    (derive-keyboard-label name "-TAIL")
     :shift-state-label (derive-keyboard-label name "-SHIFT-STATE"))))

(defun register-keyboard-driver (environment form)
  (let* ((kbd (parse-keyboard-driver-form environment form))
         (name (keyboard-descriptor-name kbd)))
    (when (gethash name (compile-environment-keyboards environment))
      (error "KEYBOARD-DRIVER ~S is already declared in this section." name))
    (setf (gethash name (compile-environment-keyboards environment)) kbd)
    kbd))

(defun emit-keyboard-driver (environment form)
  (require-bits environment 32 "KEYBOARD-DRIVER")
  (let* ((state (compile-environment-state environment))
         (name (second form))
         (kbd (lookup-keyboard environment name)))
    (emit-align state 4)
    (emit-label state name)
    (multiple-value-bind (normal-table shifted-table)
        (keyboard-layout-tables (keyboard-descriptor-scancode-map kbd))
      (emit-label state (keyboard-descriptor-table-label kbd))
      (loop for byte across normal-table do
        (emit-u8 state byte))
      (emit-label state (keyboard-descriptor-shifted-table-label kbd))
      (loop for byte across shifted-table do
        (emit-u8 state byte)))
    (emit-label state (keyboard-descriptor-buffer-label kbd))
    (loop repeat (keyboard-descriptor-buffer-size kbd) do (emit-u8 state 0))
    (emit-label state (keyboard-descriptor-head-label kbd))
    (emit-u8 state 0)
    (emit-label state (keyboard-descriptor-tail-label kbd))
    (emit-u8 state 0)
    (emit-label state (keyboard-descriptor-shift-state-label kbd))
    (emit-u8 state 0)
    (emit-keyboard-handler environment kbd)))

(defstruct (block-device-descriptor
            (:constructor make-block-device-descriptor
                (&key name type bus addressing sector-size base-port)))
  name type bus addressing sector-size base-port)

(defun parse-block-device-form (environment form)
  (let* ((name (or (second form) (error "BLOCK-DEVICE requires a name.")))
         (options (resolve-operands environment (third form)))
         (type (or (getf options :type) :ata-pio))
         (bus (or (getf options :bus) :primary))
         (addressing (or (getf options :addressing) :lba28))
         (sector-size (or (getf options :sector-size) 512)))
    (unless (eq type :ata-pio)
      (error "BLOCK-DEVICE currently only supports :ATA-PIO, got ~S." type))
    (unless (eq addressing :lba28)
      (error "BLOCK-DEVICE currently only supports :LBA28 addressing, got ~S." addressing))
    (unless (= sector-size 512)
      (error "BLOCK-DEVICE :SECTOR-SIZE must be 512, got ~S." sector-size))
    (let ((base-port (ecase bus
                       (:primary #x1F0)
                       (:secondary #x170))))
      (make-block-device-descriptor
       :name name :type type :bus bus :addressing addressing
       :sector-size sector-size :base-port base-port))))

(defun register-block-device (environment form)
  (let* ((dev (parse-block-device-form environment form))
         (name (block-device-descriptor-name dev)))
    (when (gethash name (compile-environment-block-devices environment))
      (error "BLOCK-DEVICE ~S is already declared in this section." name))
    (setf (gethash name (compile-environment-block-devices environment)) dev)
    dev))

(defun lookup-block-device (environment name)
  (or (gethash name (compile-environment-block-devices environment))
      (error "Unknown block device ~S — is (block-device ~S ...) declared in this section?"
             name name)))

(defun load-into-register (environment dest source)
  (let ((state (compile-environment-state environment)))
    (unless (and (symbolp source)
                 (or (dword-register-p source) (word-register-p source) (byte-register-p source))
                 (eql (token-keyword source) (token-keyword dest)))
      (encode-instruction state 'mov (list dest source)))))

(defun emit-ata-pio-transfer (environment dev args read-p)
  (let* ((rest-args (resolve-operands environment args))
         (lba-src (or (getf rest-args :lba)
                      (error "BLOCK-~A requires :LBA." (if read-p "READ" "WRITE"))))
         (count (or (getf rest-args :count) 1))
         (buffer-src (or (getf rest-args :buffer)
                         (error "BLOCK-~A requires :BUFFER." (if read-p "READ" "WRITE"))))
         (state (compile-environment-state environment))
         (base (block-device-descriptor-base-port dev))
         (data-port  (+ base 0))
         (count-port (+ base 2))
         (lba0-port  (+ base 3))
         (lba1-port  (+ base 4))
         (lba2-port  (+ base 5))
         (drive-port (+ base 6))
         (status-port (+ base 7))
         (cmd-port    (+ base 7))
         (poll-bsy    (gensym "ATA-BSY-"))
         (poll-drq    (gensym "ATA-DRQ-"))
         (sector-loop (gensym "ATA-SECTOR-"))
         (cmd-byte    (if read-p #x20 #x30))
         (string-op   (if read-p 'insw 'outsw))
         (string-reg  (if read-p 'edi 'esi)))
    (unless (and (integerp count) (>= count 1) (<= count 255))
      (error "BLOCK transfer :COUNT must be an integer 1..255, got ~S." count))
    (load-into-register environment 'eax lba-src)
    (load-into-register environment string-reg buffer-src)
    ;; BSY poll clobbers AL — preserve caller's EAX (which holds LBA).
    (encode-instruction state 'push '(eax))
    (encode-instruction state 'mov (list 'dx status-port))
    (emit-label state poll-bsy)
    (encode-instruction state 'in '(al dx))
    (encode-instruction state 'test '(al #x80))
    (encode-instruction state 'jnz-near (list poll-bsy))
    (encode-instruction state 'pop '(eax))
    (encode-instruction state 'mov (list 'dx count-port))
    (encode-instruction state 'push '(eax))
    (encode-instruction state 'mov (list 'al count))
    (encode-instruction state 'out '(dx al))
    (encode-instruction state 'pop '(eax))
    (encode-instruction state 'mov (list 'dx lba0-port))
    (encode-instruction state 'out '(dx al))
    (encode-instruction state 'shr '(eax 8))
    (encode-instruction state 'mov (list 'dx lba1-port))
    (encode-instruction state 'out '(dx al))
    (encode-instruction state 'shr '(eax 8))
    (encode-instruction state 'mov (list 'dx lba2-port))
    (encode-instruction state 'out '(dx al))
    (encode-instruction state 'shr '(eax 8))
    (encode-instruction state 'and '(al #x0F))
    (encode-instruction state 'or '(al #xE0))
    (encode-instruction state 'mov (list 'dx drive-port))
    (encode-instruction state 'out '(dx al))
    ;; ATA spec: after drive-select, wait >=400ns before reading status.
    ;; Read alt-status (0x3F6) 4 times — non-destructive, doesn't clear IRQ.
    (encode-instruction state 'mov (list 'dx #x3F6))
    (encode-instruction state 'in '(al dx))
    (encode-instruction state 'in '(al dx))
    (encode-instruction state 'in '(al dx))
    (encode-instruction state 'in '(al dx))
    (encode-instruction state 'mov (list 'dx cmd-port))
    (encode-instruction state 'mov (list 'al cmd-byte))
    (encode-instruction state 'out '(dx al))
    ;; 400ns delay after command — let BSY rise.
    (encode-instruction state 'mov (list 'dx #x3F6))
    (encode-instruction state 'in '(al dx))
    (encode-instruction state 'in '(al dx))
    (encode-instruction state 'in '(al dx))
    (encode-instruction state 'in '(al dx))
    (encode-instruction state 'mov (list 'ebp count))
    (encode-instruction state 'cld '())
    (emit-label state sector-loop)
    (emit-label state poll-drq)
    (encode-instruction state 'mov (list 'dx status-port))
    (encode-instruction state 'in '(al dx))
    (encode-instruction state 'test '(al #x80))
    (encode-instruction state 'jnz-near (list poll-drq))
    (encode-instruction state 'test '(al #x08))
    (encode-instruction state 'jz-near (list poll-drq))
    (encode-instruction state 'mov (list 'dx data-port))
    (encode-instruction state 'mov (list 'ecx 256))
    (encode-instruction state 'rep (list string-op))
    (encode-instruction state 'dec '(ebp))
    (encode-instruction state 'jnz-near (list sector-loop))
    (unless read-p
      ;; ATA cache flush after writes.
      (encode-instruction state 'mov (list 'dx cmd-port))
      (encode-instruction state 'mov '(al #xE7))
      (encode-instruction state 'out '(dx al))
      (encode-instruction state 'mov (list 'dx status-port))
      (let ((flush-wait (gensym "ATA-FLUSH-")))
        (emit-label state flush-wait)
        (encode-instruction state 'in '(al dx))
        (encode-instruction state 'test '(al #x80))
        (encode-instruction state 'jnz-near (list flush-wait))))))

(defun emit-block-read (environment args)
  (require-bits environment 32 "BLOCK-READ")
  (let* ((dev-name (or (first args) (error "BLOCK-READ requires a device name.")))
         (dev (lookup-block-device environment dev-name)))
    (emit-ata-pio-transfer environment dev (rest args) t)))

(defun emit-block-write (environment args)
  (require-bits environment 32 "BLOCK-WRITE")
  (let* ((dev-name (or (first args) (error "BLOCK-WRITE requires a device name.")))
         (dev (lookup-block-device environment dev-name)))
    (emit-ata-pio-transfer environment dev (rest args) nil)))

;;;; ----------------------------------------------------------------
;;;; Phase 6 — Storage and persistence
;;;;
;;;;   (partition-table NAME (:device DEV :format :mbr))
;;;;   (partition-table-read NAME)
;;;;   (partition-find NAME :type N :result-lba REG :result-count REG)
;;;;
;;;;   (volume NAME (:partition-table TBL :partition INDEX
;;;;                 :cache-buffer ADDR :cache-size N))
;;;;   (volume-read NAME :lba REG :buffer REG)
;;;;   (volume-write NAME :lba REG :buffer REG)
;;;;
;;;; The DSL is filesystem-agnostic.  Filesystem implementations live above
;;;; this layer.
;;;; ----------------------------------------------------------------

(defstruct (partition-table-descriptor
            (:constructor make-partition-table-descriptor
                (&key name device format buffer-label parsed-p)))
  name
  device
  format
  buffer-label
  ;; runtime-parsed-flag word label (set by partition-table-read on success)
  parsed-p)

(defun parse-partition-table-form (environment form)
  (let* ((name (or (second form) (error "PARTITION-TABLE requires a name.")))
         (options (resolve-operands environment (third form)))
         (device (or (getf options :device)
                     (error "PARTITION-TABLE ~S requires :DEVICE." name)))
         (format (or (getf options :format) :mbr))
         (pkg (or (symbol-package name) *package*)))
    (unless (eq format :mbr)
      (error "PARTITION-TABLE ~S only supports :MBR, got ~S." name format))
    (make-partition-table-descriptor
     :name name
     :device device
     :format format
     :buffer-label (intern (concatenate 'string (symbol-name name) "-MBR-BUFFER") pkg)
     :parsed-p (intern (concatenate 'string (symbol-name name) "-MBR-VALID") pkg))))

(defun register-partition-table (environment form)
  (let* ((tbl (parse-partition-table-form environment form))
         (name (partition-table-descriptor-name tbl)))
    (when (gethash name (compile-environment-partition-tables environment))
      (error "PARTITION-TABLE ~S is already declared in this section." name))
    (setf (gethash name (compile-environment-partition-tables environment)) tbl)
    tbl))

(defun lookup-partition-table (environment name)
  (or (gethash name (compile-environment-partition-tables environment))
      (error "Unknown PARTITION-TABLE ~S — declare (partition-table ~S ...) in this section."
             name name)))

(defun emit-partition-table (environment form)
  "Emit the runtime data block for a partition table: 512-byte sector buffer
plus a 4-byte parsed flag.  Pure data — no bit-mode requirement.
Code lives in PARTITION-TABLE-READ etc."
  (let* ((state (compile-environment-state environment))
         (name (second form))
         (tbl (lookup-partition-table environment name)))
    (emit-align state 4)
    (emit-label state name)
    (emit-label state (partition-table-descriptor-buffer-label tbl))
    (loop repeat 512 do (emit-u8 state 0))
    (emit-label state (partition-table-descriptor-parsed-p tbl))
    (emit-u8 state 0)
    (emit-u8 state 0)
    (emit-u8 state 0)
    (emit-u8 state 0)))

(defun emit-partition-table-read (environment args)
  "Read a sector containing a partition table from the device into the
partition-table buffer and validate the 0xAA55 signature.  Default LBA is 0
(MBR convention); override with :LBA.  On success sets AL=1 and the
descriptor's parsed flag; on failure sets AL=0.  ZF reflects AL (caller
branches with JZ for failure)."
  (require-bits environment 32 "PARTITION-TABLE-READ")
  (let* ((tbl-name (or (first args)
                       (error "PARTITION-TABLE-READ requires a table name.")))
         (rest-args (resolve-operands environment (rest args)))
         (lba (getf rest-args :lba 0))
         (tbl (lookup-partition-table environment tbl-name))
         (state (compile-environment-state environment))
         (buffer (partition-table-descriptor-buffer-label tbl))
         (parsed (partition-table-descriptor-parsed-p tbl))
         (device (partition-table-descriptor-device tbl))
         (fail-label (gensym "PT-FAIL-"))
         (done-label (gensym "PT-DONE-")))
    (emit-block-read environment
                     (list device
                           :lba lba
                           :count 1
                           :buffer buffer))
    (encode-instruction state 'mov (list 'eax buffer))
    (encode-instruction state 'mov (list 'ax (list :MEM :word 'eax 510)))
    (encode-instruction state 'cmp (list 'ax #xAA55))
    (encode-instruction state 'jne-near (list fail-label))
    (encode-instruction state 'mov (list (list :MEM :byte parsed) 1))
    (encode-instruction state 'mov '(al 1))
    (encode-instruction state 'jmp (list done-label))
    (emit-label state fail-label)
    (encode-instruction state 'mov (list (list :MEM :byte parsed) 0))
    (encode-instruction state 'xor '(al al))
    (emit-label state done-label)
    (encode-instruction state 'test '(al al))))

(defparameter +mbr-partition-table-offset+ #x1BE)
(defparameter +mbr-partition-entry-size+ 16)
(defparameter +mbr-partition-entry-count+ 4)

(defun emit-partition-find (environment args)
  "Scan the four MBR partition entries for one whose type byte equals :TYPE.
On match: :RESULT-LBA <- entry's start LBA, :RESULT-COUNT <- entry's count,
AL=1, ZF=0.  On miss: registers untouched, AL=0, ZF=1.
Clobbers ESI."
  (require-bits environment 32 "PARTITION-FIND")
  (let* ((tbl-name (or (first args)
                       (error "PARTITION-FIND requires a table name as the first argument.")))
         (rest-args (resolve-operands environment (rest args)))
         (tbl (lookup-partition-table environment tbl-name))
         (type-byte (or (getf rest-args :type)
                        (error "PARTITION-FIND requires :TYPE.")))
         (result-lba (or (getf rest-args :result-lba)
                         (error "PARTITION-FIND requires :RESULT-LBA register.")))
         (result-count (or (getf rest-args :result-count)
                           (error "PARTITION-FIND requires :RESULT-COUNT register.")))
         (state (compile-environment-state environment))
         (buffer (partition-table-descriptor-buffer-label tbl))
         (loop-label (gensym "PT-FIND-LOOP-"))
         (next-label (gensym "PT-FIND-NEXT-"))
         (found-label (gensym "PT-FIND-OK-"))
         (miss-label (gensym "PT-FIND-MISS-"))
         (done-label (gensym "PT-FIND-DONE-")))
    (unless (and (integerp type-byte) (typep type-byte '(integer 0 255)))
      (error "PARTITION-FIND :TYPE must be an unsigned byte, got ~S." type-byte))
    (unless (dword-register-p result-lba)
      (error "PARTITION-FIND :RESULT-LBA must be a 32-bit register, got ~S." result-lba))
    (unless (dword-register-p result-count)
      (error "PARTITION-FIND :RESULT-COUNT must be a 32-bit register, got ~S." result-count))
    (encode-instruction state 'mov (list 'esi buffer))
    (encode-instruction state 'add (list 'esi +mbr-partition-table-offset+))
    (encode-instruction state 'mov (list 'ecx +mbr-partition-entry-count+))
    (emit-label state loop-label)
    (encode-instruction state 'mov '(al (:mem :byte esi 4)))   ; type byte at +4
    (encode-instruction state 'cmp (list 'al type-byte))
    (encode-instruction state 'je-near (list found-label))
    (emit-label state next-label)
    (encode-instruction state 'add (list 'esi +mbr-partition-entry-size+))
    (encode-instruction state 'dec '(ecx))
    (encode-instruction state 'jnz-near (list loop-label))
    (emit-label state miss-label)
    (encode-instruction state 'xor '(al al))
    (encode-instruction state 'jmp (list done-label))
    (emit-label state found-label)
    (encode-instruction state 'mov (list result-lba (list :MEM :dword 'esi 8)))
    (encode-instruction state 'mov (list result-count (list :MEM :dword 'esi 12)))
    (encode-instruction state 'mov '(al 1))
    (emit-label state done-label)
    (encode-instruction state 'test '(al al))))

(defstruct (volume-descriptor
            (:constructor make-volume-descriptor
                (&key name partition-table partition-index device
                      cache-buffer cache-size start-lba-label)))
  name
  partition-table
  partition-index
  device
  cache-buffer
  cache-size
  start-lba-label)

(defun parse-volume-form (environment form)
  (let* ((name (or (second form) (error "VOLUME requires a name.")))
         (options (resolve-operands environment (third form)))
         (table-name (getf options :partition-table))
         (partition-index (getf options :partition))
         (device (getf options :device))
         (cache-buffer (getf options :cache-buffer))
         (cache-size (or (getf options :cache-size) 512))
         (pkg (or (symbol-package name) *package*)))
    (unless (or device table-name)
      (error "VOLUME ~S requires :DEVICE or :PARTITION-TABLE." name))
    (when (and table-name partition-index
               (not (and (integerp partition-index)
                         (typep partition-index '(integer 0 3)))))
      (error "VOLUME ~S :PARTITION must be 0..3 for MBR, got ~S." name partition-index))
    (make-volume-descriptor
     :name name
     :partition-table table-name
     :partition-index partition-index
     :device device
     :cache-buffer cache-buffer
     :cache-size cache-size
     :start-lba-label (intern (concatenate 'string (symbol-name name) "-START-LBA") pkg))))

(defun register-volume (environment form)
  (let* ((vol (parse-volume-form environment form))
         (name (volume-descriptor-name vol)))
    (when (gethash name (compile-environment-volumes environment))
      (error "VOLUME ~S is already declared in this section." name))
    (setf (gethash name (compile-environment-volumes environment)) vol)
    vol))

(defun lookup-volume (environment name)
  (or (gethash name (compile-environment-volumes environment))
      (error "Unknown VOLUME ~S — declare (volume ~S ...) in this section."
             name name)))

(defun volume-effective-device (environment vol)
  (or (volume-descriptor-device vol)
      (let ((tbl-name (volume-descriptor-partition-table vol)))
        (and tbl-name
             (partition-table-descriptor-device
              (lookup-partition-table environment tbl-name))))
      (error "VOLUME ~S has neither :DEVICE nor reachable :PARTITION-TABLE."
             (volume-descriptor-name vol))))

(defun emit-volume (environment form)
  "Emit the runtime metadata for a volume: a 4-byte START-LBA cell.
Set by VOLUME-BIND from the parsed partition table.  Pure data — no bit-mode
requirement.  Cache buffer is left to the OS author."
  (let* ((state (compile-environment-state environment))
         (name (second form))
         (vol (lookup-volume environment name)))
    (emit-align state 4)
    (emit-label state name)
    (emit-label state (volume-descriptor-start-lba-label vol))
    (emit-u8 state 0) (emit-u8 state 0) (emit-u8 state 0) (emit-u8 state 0)))

(defun emit-volume-bind (environment args)
  "Copy a volume's partition start LBA from the parsed sector buffer into the
volume's runtime START-LBA cell."
  (require-bits environment 32 "VOLUME-BIND")
  (let* ((vol-name (or (first args)
                       (error "VOLUME-BIND requires a volume name as the first argument.")))
         (vol (lookup-volume environment vol-name))
         (state (compile-environment-state environment))
         (tbl-name (or (volume-descriptor-partition-table vol)
                       (error "VOLUME ~S has no :PARTITION-TABLE — cannot bind." vol-name)))
         (tbl (lookup-partition-table environment tbl-name))
         (idx (or (volume-descriptor-partition-index vol)
                  (error "VOLUME ~S requires :PARTITION index for binding." vol-name)))
         (entry-offset (+ +mbr-partition-table-offset+
                          (* idx +mbr-partition-entry-size+)
                          8)) ; +8 = LBA-start field
         (start-cell (volume-descriptor-start-lba-label vol)))
    (encode-instruction state 'mov (list 'esi (partition-table-descriptor-buffer-label tbl)))
    (encode-instruction state 'mov (list 'eax (list :MEM :dword 'esi entry-offset)))
    (encode-instruction state 'mov (list (list :MEM :dword start-cell) 'eax))))

(defun emit-volume-transfer (environment args read-p)
  (require-bits environment 32 (if read-p "VOLUME-READ" "VOLUME-WRITE"))
  (let* ((args (resolve-operands environment args))
         (vol-name (or (first args)
                       (error "VOLUME-~A requires a volume name."
                              (if read-p "READ" "WRITE"))))
         (vol (lookup-volume environment vol-name))
         (rest-args (rest args))
         (lba-src (or (getf rest-args :lba)
                      (error "VOLUME-~A requires :LBA." (if read-p "READ" "WRITE"))))
         (count (or (getf rest-args :count) 1))
         (buffer-src (or (getf rest-args :buffer)
                         (error "VOLUME-~A requires :BUFFER." (if read-p "READ" "WRITE"))))
         (state (compile-environment-state environment))
         (device (volume-effective-device environment vol))
         (start-cell (volume-descriptor-start-lba-label vol)))
    (load-into-register environment 'eax lba-src)
    (when (volume-descriptor-partition-table vol)
      (encode-instruction state 'add (list 'eax (list :MEM :dword start-cell))))
    (if read-p
        (emit-block-read environment
                         (list device :lba 'eax :count count :buffer buffer-src))
        (emit-block-write environment
                          (list device :lba 'eax :count count :buffer buffer-src)))))

(defun emit-volume-read (environment args)
  (emit-volume-transfer environment args t))

(defun emit-volume-write (environment args)
  (emit-volume-transfer environment args nil))

(defstruct (memory-map-descriptor
            (:constructor make-memory-map-descriptor
                (&key name source max-entries entries-label reserved-ranges)))
  name source max-entries entries-label reserved-ranges)

(defun parse-memory-map-form (environment form)
  (let* ((name (or (second form) (error "MEMORY-MAP requires a name.")))
         (rest (cddr form)))
    (multiple-value-bind (raw-options entries)
        (parse-leading-options rest '(:source :max-entries))
      (let* ((options (resolve-operands environment raw-options))
         (source (or (getf options :source) :bios-e820))
             (max-entries (or (getf options :max-entries) 32))
             (reserved-ranges nil))
        (unless (eq source :bios-e820)
          (error "MEMORY-MAP currently only supports :BIOS-E820, got ~S." source))
        (unless (and (integerp max-entries) (>= max-entries 1) (<= max-entries 256))
          (error "MEMORY-MAP :MAX-ENTRIES must be 1..256, got ~S." max-entries))
        (dolist (entry entries)
          (destructuring-bind (kind start end) (resolve-operands environment entry)
            (unless (eq (token-keyword kind) :reserved)
              (error "Unsupported MEMORY-MAP entry form ~S." kind))
            (unless (and (integerp start)
                         (integerp end)
                         (<= 0 start end #xFFFFFFFF))
              (error "MEMORY-MAP reserved range must be 32-bit ascending addresses, got ~S .. ~S."
                     start end))
            (push (cons start end) reserved-ranges)))
        (make-memory-map-descriptor
         :name name
         :source source
         :max-entries max-entries
         :entries-label (intern (concatenate 'string (symbol-name name) "-ENTRIES")
                                (or (symbol-package name) *package*))
         :reserved-ranges (nreverse reserved-ranges))))))

(defun register-memory-map (environment form)
  (let* ((mm (parse-memory-map-form environment form))
         (name (memory-map-descriptor-name mm)))
    (when (gethash name (compile-environment-memory-maps environment))
      (error "MEMORY-MAP ~S is already declared in this section." name))
    (setf (gethash name (compile-environment-memory-maps environment)) mm)
    mm))

(defun lookup-memory-map (environment name)
  (or (gethash name (compile-environment-memory-maps environment))
      (error "Unknown memory map ~S — is (memory-map ~S ...) declared in this section?"
             name name)))

(defun emit-memory-map (environment form)
  (let* ((state (compile-environment-state environment))
         (name (second form))
         (mm (lookup-memory-map environment name)))
    (emit-align state 4)
    (emit-label state name)
    (emit-dd state '(0))                ; count dword (zeroed at boot)
    (emit-label state (memory-map-descriptor-entries-label mm))
    (loop repeat (* 24 (memory-map-descriptor-max-entries mm))
          do (emit-u8 state 0))))

(defun emit-memory-map-probe (environment args)
  (require-bits environment 16 "MEMORY-MAP-PROBE")
  (let* ((mm-name (or (first args)
                      (error "MEMORY-MAP-PROBE requires a memory-map name.")))
         (mm (lookup-memory-map environment mm-name))
         (state (compile-environment-state environment))
         (entries (memory-map-descriptor-entries-label mm))
         (count-label mm-name)
         (loop-label (gensym "E820-LOOP-"))
         (done-label (gensym "E820-DONE-")))
    (encode-instruction state 'xor '(ebx ebx))
    (encode-instruction state 'mov (list 'edi entries))
    (encode-instruction state 'xor '(ebp ebp))
    (emit-label state loop-label)
    (encode-instruction state 'mov (list 'eax #x0000E820))
    (encode-instruction state 'mov (list 'edx #x534D4150))
    (encode-instruction state 'mov '(ecx 24))
    (encode-instruction state 'int '(#x15))
    (encode-instruction state 'jc (list done-label))
    (encode-instruction state 'cmp (list 'eax #x534D4150))
    (encode-instruction state 'jne (list done-label))
    (encode-instruction state 'add '(edi 24))
    (encode-instruction state 'inc '(ebp))
    (encode-instruction state 'test '(ebx ebx))
    (encode-instruction state 'jnz (list loop-label))
    (emit-label state done-label)
    (encode-instruction state 'mov (list (list :MEM :DWORD count-label) 'ebp))))

(defun emit-memory-map-base (environment args)
  (let* ((dest (or (first args) (error "MEMORY-MAP-BASE requires a destination register.")))
         (mm-name (or (second args) (error "MEMORY-MAP-BASE requires a memory-map name.")))
         (mm (lookup-memory-map environment mm-name))
         (state (compile-environment-state environment)))
    (encode-instruction state 'mov (list dest (memory-map-descriptor-entries-label mm)))))

(defun emit-memory-map-count (environment args)
  (let* ((dest (or (first args) (error "MEMORY-MAP-COUNT requires a destination register.")))
         (mm-name (or (second args) (error "MEMORY-MAP-COUNT requires a memory-map name.")))
         (mm (lookup-memory-map environment mm-name))
         (state (compile-environment-state environment)))
    (encode-instruction state 'mov (list dest (list :MEM :DWORD (memory-map-descriptor-name mm))))))

(defun memory-map-type-value (designator)
  (let ((designator (unwrap-quoted-value designator)))
    (cond ((or (null designator) (eq designator t))
           nil)
          ((integerp designator)
           designator)
          ((symbolp designator)
           (case (token-keyword designator)
             (:available 1)
             (:reserved 2)
             (:acpi-reclaimable 3)
             (:acpi-nvs 4)
             (:bad 5)
             (t
              (error "Unknown MEMORY-MAP-ITERATE :TYPE designator ~S." designator))))
          (t
           (error "Unsupported MEMORY-MAP-ITERATE :TYPE designator ~S." designator)))))

(defun parse-memory-map-iterate-args (environment args)
  (let* ((body-position (position-if (lambda (item)
                                       (and (typep item '(or symbol string))
                                            (eq (token-keyword item) :body)))
                                     args))
         (mm-name (or (first args)
                      (error "MEMORY-MAP-ITERATE requires a memory-map name."))))
    (when (null body-position)
      (error "MEMORY-MAP-ITERATE requires a :BODY clause."))
    (let* ((option-items (subseq args 1 body-position))
           (body-items (subseq args (1+ body-position)))
           (options (resolve-operands environment option-items))
           (body (if (and (= (length body-items) 1)
                          (consp (first body-items))
                          (every #'consp (first body-items)))
                     (first body-items)
                     body-items)))
      (values mm-name options body))))

(defun emit-memory-map-iterate (environment args)
  (require-bits environment 32 "MEMORY-MAP-ITERATE")
  (multiple-value-bind (mm-name options body)
      (parse-memory-map-iterate-args environment args)
    (let* ((state (compile-environment-state environment))
           (mm (lookup-memory-map environment mm-name))
           (base-reg (or (getf options :base-reg)
                         (error "MEMORY-MAP-ITERATE requires :BASE-REG.")))
           (length-reg (or (getf options :length-reg)
                           (error "MEMORY-MAP-ITERATE requires :LENGTH-REG.")))
           (entry-reg (getf options :entry-reg))
           (type-reg (getf options :type-reg))
           (type-filter (memory-map-type-value (getf options :type)))
           (loop-label (gensym "MMAP-ITERATE-"))
           (skip-label (gensym "MMAP-SKIP-"))
           (done-label (gensym "MMAP-DONE-")))
      (dolist (reg (remove nil (list base-reg length-reg entry-reg type-reg)))
        (unless (dword-register-p reg)
          (error "MEMORY-MAP-ITERATE registers must be 32-bit general registers, got ~S." reg))
        (when (member reg '(edi ebp))
          (error "MEMORY-MAP-ITERATE reserves EDI and EBP for loop state; got ~S." reg)))
      (encode-instruction state 'mov (list 'edi (memory-map-descriptor-entries-label mm)))
      (encode-instruction state 'mov (list 'ebp (list :MEM :DWORD (memory-map-descriptor-name mm))))
      (emit-label state loop-label)
      (encode-instruction state 'test '(ebp ebp))
      (encode-instruction state 'jz (list done-label))
      (when type-filter
        (encode-instruction state 'cmp (list (list :MEM :DWORD 'edi 16) type-filter))
        (encode-instruction state 'jne-near (list skip-label)))
      (encode-instruction state 'mov (list base-reg (list :MEM :DWORD 'edi 0)))
      (encode-instruction state 'mov (list length-reg (list :MEM :DWORD 'edi 8)))
      (when entry-reg
        (encode-instruction state 'mov (list entry-reg 'edi)))
      (when type-reg
        (encode-instruction state 'mov (list type-reg (list :MEM :DWORD 'edi 16))))
      (dolist (nested-form body)
        (compile-form environment nested-form))
      (emit-label state skip-label)
      (encode-instruction state 'add '(edi 24))
      (encode-instruction state 'dec '(ebp))
      (encode-instruction state 'jmp (list loop-label))
      (emit-label state done-label))))

(defstruct (phys-allocator-descriptor
            (:constructor make-phys-allocator-descriptor
                (&key name base-address page-size max-pages
                      bitmap-label alloc-helper free-helper)))
  name base-address page-size max-pages
  bitmap-label alloc-helper free-helper)

(defun parse-phys-allocator-form (environment form)
  (let* ((name (or (second form) (error "PHYS-ALLOCATOR-BOOTSTRAP requires a name.")))
         (options (resolve-operands environment (third form)))
         (base-address (or (getf options :base-address) 0))
         (page-size (or (getf options :page-size) 4096))
         (max-pages (or (getf options :max-pages) 4096)))
    (unless (and (integerp base-address) (>= base-address 0))
      (error "PHYS-ALLOCATOR-BOOTSTRAP :BASE-ADDRESS must be a non-negative integer, got ~S."
             base-address))
    (unless (and (integerp page-size)
                 (plusp page-size)
                 (zerop (logand page-size (1- page-size))))
      (error "PHYS-ALLOCATOR-BOOTSTRAP :PAGE-SIZE must be a positive power of two, got ~S."
             page-size))
    (unless (and (integerp max-pages)
                 (>= max-pages 8)
                 (zerop (mod max-pages 8)))
      (error "PHYS-ALLOCATOR-BOOTSTRAP :MAX-PAGES must be a positive multiple of 8, got ~S."
             max-pages))
    (let ((pkg (or (symbol-package name) *package*)))
      (make-phys-allocator-descriptor
       :name name
       :base-address base-address
       :page-size page-size
       :max-pages max-pages
       :bitmap-label  (intern (concatenate 'string (symbol-name name) "-BITMAP") pkg)
       :alloc-helper  (intern (concatenate 'string (symbol-name name) "-ALLOC-HELPER") pkg)
       :free-helper   (intern (concatenate 'string (symbol-name name) "-FREE-HELPER") pkg)))))

(defun register-phys-allocator (environment form)
  (let* ((alloc (parse-phys-allocator-form environment form))
         (name (phys-allocator-descriptor-name alloc)))
    (when (gethash name (compile-environment-phys-allocators environment))
      (error "PHYS-ALLOCATOR-BOOTSTRAP ~S is already declared in this section." name))
    (setf (gethash name (compile-environment-phys-allocators environment)) alloc)
    alloc))

(defun lookup-phys-allocator (environment name)
  (or (gethash name (compile-environment-phys-allocators environment))
      (error "Unknown phys-allocator ~S — is (phys-allocator-bootstrap ~S ...) declared in this section?"
             name name)))

(defun emit-phys-alloc-helper (environment alloc)
  (let* ((state (compile-environment-state environment))
         (bytes (truncate (phys-allocator-descriptor-max-pages alloc) 8))
         (bitmap (phys-allocator-descriptor-bitmap-label alloc))
         (page-shift (integer-length (1- (phys-allocator-descriptor-page-size alloc))))
         (base-addr (phys-allocator-descriptor-base-address alloc))
         (scan-loop (gensym "PA-SCAN-"))
         (found-byte (gensym "PA-FOUND-"))
         (bit-loop (gensym "PA-BITS-"))
         (bit-found (gensym "PA-BIT-OK-"))
         (alloc-fail (gensym "PA-FAIL-"))
         (alloc-done (gensym "PA-DONE-")))
    (emit-label state (phys-allocator-descriptor-alloc-helper alloc))
    (encode-instruction state 'mov (list 'esi bitmap))
    (encode-instruction state 'mov (list 'ecx bytes))
    (emit-label state scan-loop)
    (encode-instruction state 'test '(ecx ecx))
    (encode-instruction state 'jz-near (list alloc-fail))
    (encode-instruction state 'mov '(al (:mem :byte esi)))
    (encode-instruction state 'cmp '(al #xFF))
    (encode-instruction state 'jne-near (list found-byte))
    (encode-instruction state 'inc '(esi))
    (encode-instruction state 'dec '(ecx))
    (encode-instruction state 'jmp (list scan-loop))
    (emit-label state found-byte)
    (encode-instruction state 'xor '(bl bl))   ; bit index
    (encode-instruction state 'mov '(ah 1))    ; mask
    (emit-label state bit-loop)
    (encode-instruction state 'test '(al ah))
    (encode-instruction state 'jz-near (list bit-found))
    (encode-instruction state 'inc '(bl))
    (encode-instruction state 'shl '(ah 1))
    (encode-instruction state 'jmp (list bit-loop))
    (emit-label state bit-found)
    (encode-instruction state 'or '(al ah))
    (encode-instruction state 'mov '((:mem :byte esi) al))
    (encode-instruction state 'mov '(eax esi))
    (encode-instruction state 'sub (list 'eax bitmap))
    (encode-instruction state 'shl '(eax 3))
    (encode-instruction state 'movzx '(ecx bl))
    (encode-instruction state 'add '(eax ecx))
    (encode-instruction state 'shl (list 'eax page-shift))
    (when (plusp base-addr)
      (encode-instruction state 'add (list 'eax base-addr)))
    (encode-instruction state 'jmp (list alloc-done))
    (emit-label state alloc-fail)
    (encode-instruction state 'xor '(eax eax))
    (emit-label state alloc-done)
    (encode-instruction state 'ret '())))

(defun emit-phys-free-helper (environment alloc)
  (let* ((state (compile-environment-state environment))
         (bitmap (phys-allocator-descriptor-bitmap-label alloc))
         (page-shift (integer-length (1- (phys-allocator-descriptor-page-size alloc))))
         (base-addr (phys-allocator-descriptor-base-address alloc)))
    (emit-label state (phys-allocator-descriptor-free-helper alloc))
    ;; EAX = address to free.
    (when (plusp base-addr)
      (encode-instruction state 'sub (list 'eax base-addr)))
    (encode-instruction state 'shr (list 'eax page-shift))
    ;; EAX = page index. Compute byte offset (EAX >> 3) and bit (EAX & 7).
    (encode-instruction state 'mov '(ebx eax))
    (encode-instruction state 'shr '(ebx 3))
    (encode-instruction state 'and '(eax 7))
    (encode-instruction state 'mov '(cl al))
    (encode-instruction state 'mov '(al 1))
    (encode-instruction state 'shl '(al cl))
    (encode-instruction state 'not '(al))
    ;; ESI = bitmap[byte-offset]
    (encode-instruction state 'mov (list 'esi bitmap))
    (encode-instruction state 'add '(esi ebx))
    (encode-instruction state 'and '((:mem :byte esi) al))
    (encode-instruction state 'ret '())))

(defun emit-phys-allocator-bootstrap (environment form)
  (require-bits environment 32 "PHYS-ALLOCATOR-BOOTSTRAP")
  (let* ((state (compile-environment-state environment))
         (name (second form))
         (alloc (lookup-phys-allocator environment name))
         (bytes (truncate (phys-allocator-descriptor-max-pages alloc) 8)))
    (emit-align state 4)
    (emit-label state name)
    (emit-label state (phys-allocator-descriptor-bitmap-label alloc))
    (loop repeat bytes do (emit-u8 state 0))
    (emit-phys-alloc-helper environment alloc)
    (emit-phys-free-helper environment alloc)))

(defun emit-phys-allocator-init (environment args)
  (require-bits environment 32 "PHYS-ALLOCATOR-INIT")
  (let* ((name (or (first args) (error "PHYS-ALLOCATOR-INIT requires an allocator name.")))
         (alloc (lookup-phys-allocator environment name))
         (state (compile-environment-state environment))
         (dwords (truncate (truncate (phys-allocator-descriptor-max-pages alloc) 8) 4)))
    (encode-instruction state 'mov (list 'edi (phys-allocator-descriptor-bitmap-label alloc)))
    (encode-instruction state 'mov (list 'ecx dwords))
    (encode-instruction state 'xor '(eax eax))
    (encode-instruction state 'cld '())
    (encode-instruction state 'rep '(stosd))))

(defun emit-phys-alloc (environment args)
  (require-bits environment 32 "PHYS-ALLOC")
  (let* ((name (or (first args) (error "PHYS-ALLOC requires an allocator name.")))
         (dest (or (second args) (error "PHYS-ALLOC requires a destination register.")))
         (alloc (lookup-phys-allocator environment name))
         (state (compile-environment-state environment)))
    (unless (dword-register-p dest)
      (error "PHYS-ALLOC destination must be a 32-bit register, got ~S." dest))
    (encode-instruction state 'call (list (phys-allocator-descriptor-alloc-helper alloc)))
    (unless (eql (token-keyword dest) :EAX)
      (encode-instruction state 'mov (list dest 'eax)))))

(defun emit-phys-free (environment args)
  (require-bits environment 32 "PHYS-FREE")
  (let* ((name (or (first args) (error "PHYS-FREE requires an allocator name.")))
         (src (or (second args) (error "PHYS-FREE requires a source register.")))
         (alloc (lookup-phys-allocator environment name))
         (state (compile-environment-state environment)))
    (unless (dword-register-p src)
      (error "PHYS-FREE source must be a 32-bit register, got ~S." src))
    (unless (eql (token-keyword src) :EAX)
      (encode-instruction state 'mov (list 'eax src)))
    (encode-instruction state 'call (list (phys-allocator-descriptor-free-helper alloc)))))

(defparameter +execution-slot-bytes+ 24)

(defstruct (slot-type-descriptor
            (:constructor make-slot-type-descriptor
                (&key name extra-fields total-bytes)))
  name extra-fields total-bytes)

(defun slot-field-width (kind)
  (ecase (token-keyword kind)
    (:uint8 1)
    (:uint16 2)
    (:uint32 4)
    (:uint64 8)))

(defun parse-slot-type-form (form)
  (let* ((name (or (second form)
                   (error "EXECUTION-SLOT-TYPE requires a name.")))
         (clauses (cddr form))
         (extras nil))
    (dolist (clause clauses)
      (let ((kind (token-keyword (first clause))))
        (case kind
          (:extra-fields
           (dolist (field-form (rest clause))
             (destructuring-bind (field-kw field-name field-type) field-form
               (unless (eq (token-keyword field-kw) :field)
                 (error "EXECUTION-SLOT-TYPE field clause must start with :FIELD, got ~S." field-kw))
               (push (cons field-name (slot-field-width field-type)) extras))))
          (t
           (error "Unsupported EXECUTION-SLOT-TYPE clause ~S." (first clause))))))
    (let* ((extras (nreverse extras))
           (extras-bytes (reduce #'+ extras :key #'cdr :initial-value 0))
           (total (+ +execution-slot-bytes+ extras-bytes)))
      (make-slot-type-descriptor :name name
                                 :extra-fields extras
                                 :total-bytes total))))

(defun register-slot-type (environment form)
  (let* ((descriptor (parse-slot-type-form form))
         (name (slot-type-descriptor-name descriptor)))
    (when (gethash name (compile-environment-slot-types environment))
      (error "EXECUTION-SLOT-TYPE ~S already declared in this section." name))
    (setf (gethash name (compile-environment-slot-types environment)) descriptor)
    descriptor))

(defun lookup-slot-type (environment name)
  (or (gethash name (compile-environment-slot-types environment))
      (error "Unknown EXECUTION-SLOT-TYPE ~S — declare (execution-slot-type ~S ...) in this section."
             name name)))

(defun emit-execution-slot-type (environment form)
  ;; Pre-scan registers; nothing to emit at form site.
  (declare (ignore environment form))
  nil)

(defun emit-execution-slot (environment form)
  (let* ((state (compile-environment-state environment))
         (name (or (second form)
                   (error "EXECUTION-SLOT requires a name.")))
         (options (resolve-operands environment (cddr form)))
         (type-name (getf options :type))
         (bytes (if type-name
                    (slot-type-descriptor-total-bytes
                     (lookup-slot-type environment type-name))
                    +execution-slot-bytes+)))
    (emit-align state 4)
    (emit-label state name)
    (loop repeat bytes do (emit-u8 state 0))))

(defun emit-context-save (environment args)
  (require-bits environment 32 "CONTEXT-SAVE")
  (let* ((slot-reg (or (first args)
                       (error "CONTEXT-SAVE requires a slot pointer register.")))
         (state (compile-environment-state environment))
         (after-label (gensym "CTX-SAVE-")))
    (unless (dword-register-p slot-reg)
      (error "CONTEXT-SAVE requires a 32-bit register, got ~S." slot-reg))
    (encode-instruction state 'mov (list (list :MEM :DWORD slot-reg 0) 'ebx))
    (encode-instruction state 'mov (list (list :MEM :DWORD slot-reg 4) 'esi))
    (encode-instruction state 'mov (list (list :MEM :DWORD slot-reg 8) 'edi))
    (encode-instruction state 'mov (list (list :MEM :DWORD slot-reg 12) 'ebp))
    (encode-instruction state 'mov (list (list :MEM :DWORD slot-reg 16) 'esp))
    (encode-instruction state 'mov (list (list :MEM :DWORD slot-reg 20) after-label))
    (emit-label state after-label)))

(defun emit-context-restore (environment args)
  (require-bits environment 32 "CONTEXT-RESTORE")
  (let* ((slot-reg (or (first args)
                       (error "CONTEXT-RESTORE requires a slot pointer register.")))
         (state (compile-environment-state environment)))
    (unless (dword-register-p slot-reg)
      (error "CONTEXT-RESTORE requires a 32-bit register, got ~S." slot-reg))
    (encode-instruction state 'mov (list 'ebx (list :MEM :DWORD slot-reg 0)))
    (encode-instruction state 'mov (list 'esi (list :MEM :DWORD slot-reg 4)))
    (encode-instruction state 'mov (list 'edi (list :MEM :DWORD slot-reg 8)))
    (encode-instruction state 'mov (list 'ebp (list :MEM :DWORD slot-reg 12)))
    (encode-instruction state 'mov (list 'esp (list :MEM :DWORD slot-reg 16)))
    (encode-instruction state 'jmp (list (list :MEM :DWORD slot-reg 20)))))

(defun emit-context-init (environment args)
  (require-bits environment 32 "CONTEXT-INIT")
  (let* ((slot-reg (or (first args)
                       (error "CONTEXT-INIT requires a slot pointer register.")))
         (rest-args (resolve-operands environment (rest args)))
         (entry-point (or (getf rest-args :entry-point)
                          (error "CONTEXT-INIT requires :ENTRY-POINT.")))
         (stack-top (or (getf rest-args :stack-top)
                        (error "CONTEXT-INIT requires :STACK-TOP.")))
         (state (compile-environment-state environment)))
    (unless (dword-register-p slot-reg)
      (error "CONTEXT-INIT requires a 32-bit register, got ~S." slot-reg))
    (encode-instruction state 'mov (list (list :MEM :DWORD slot-reg 0) 0))
    (encode-instruction state 'mov (list (list :MEM :DWORD slot-reg 4) 0))
    (encode-instruction state 'mov (list (list :MEM :DWORD slot-reg 8) 0))
    (encode-instruction state 'mov (list (list :MEM :DWORD slot-reg 12) 0))
    (encode-instruction state 'mov (list (list :MEM :DWORD slot-reg 16) stack-top))
    (encode-instruction state 'mov (list (list :MEM :DWORD slot-reg 20) entry-point))))

(defstruct (syscall-table-descriptor
            (:constructor make-syscall-table-descriptor
                (&key name handler-label table-label entries count)))
  name handler-label table-label entries count)

(defun parse-syscall-table-form (environment form)
  (let* ((name (or (second form) (error "SYSCALL-TABLE requires a name.")))
         (entries (cddr form))
         (parsed (loop for entry in entries
                       collect (destructuring-bind (kind vector target) entry
                                 (unless (eq (token-keyword kind) :entry)
                                   (error "SYSCALL-TABLE entry kind must be :ENTRY, got ~S." kind))
                                 (let ((vec (resolve-operand environment vector)))
                                   (unless (and (integerp vec) (>= vec 0) (<= vec 255))
                                     (error "SYSCALL-TABLE :ENTRY vector must be 0..255, got ~S." vec))
                                   (cons vec (resolve-operand environment target))))))
         (max-vec (reduce #'max (mapcar #'car parsed) :initial-value -1))
         (count (1+ max-vec))
         (pkg (or (symbol-package name) *package*)))
    (when (zerop count)
      (error "SYSCALL-TABLE ~S requires at least one :ENTRY." name))
    (make-syscall-table-descriptor
     :name name
     :handler-label (intern (concatenate 'string (symbol-name name) "-HANDLER") pkg)
     :table-label   (intern (concatenate 'string (symbol-name name) "-TABLE") pkg)
     :entries parsed
     :count count)))

(defun register-syscall-table (environment form)
  (let* ((st (parse-syscall-table-form environment form))
         (name (syscall-table-descriptor-name st)))
    (when (gethash name (compile-environment-syscall-tables environment))
      (error "SYSCALL-TABLE ~S is already declared in this section." name))
    (setf (gethash name (compile-environment-syscall-tables environment)) st)
    st))

(defun lookup-syscall-table (environment name)
  (or (gethash name (compile-environment-syscall-tables environment))
      (error "Unknown syscall-table ~S — is (syscall-table ~S ...) declared in this section?"
             name name)))

(defun emit-syscall-table (environment form)
  (require-bits environment 32 "SYSCALL-TABLE")
  (let* ((state (compile-environment-state environment))
         (name (second form))
         (st (lookup-syscall-table environment name))
         (handler (syscall-table-descriptor-handler-label st))
         (table (syscall-table-descriptor-table-label st))
         (count (syscall-table-descriptor-count st))
         (bad-label (gensym "SYS-BAD-")))
    (emit-label state name)
    (emit-label state handler)
    (encode-instruction state 'pushad '())
    (encode-instruction state 'cmp (list 'eax count))
    (encode-instruction state 'jae-near (list bad-label))
    (encode-instruction state 'mov (list 'ebx table))
    (encode-instruction state 'call (list (list :MEM :DWORD 'ebx 'eax 4 0)))
    (encode-instruction state 'popad '())
    (encode-instruction state 'iret '())
    (emit-label state bad-label)
    (encode-instruction state 'popad '())
    (encode-instruction state 'iret '())
    (emit-align state 4)
    (emit-label state table)
    (let ((index-map (make-hash-table :test #'eql)))
      (dolist (entry (syscall-table-descriptor-entries st))
        (setf (gethash (car entry) index-map) (cdr entry)))
      (dotimes (idx count)
        (let ((target (gethash idx index-map)))
          (emit-dd state (list (or target bad-label))))))))

(defun pre-scan-section (environment body)
  (dolist (form body)
    (when (consp form)
      (case (token-keyword (first form))
        (:keyboard-driver
         (register-keyboard-driver environment form))
        (:block-device
         (register-block-device environment form))
        (:memory-map
         (register-memory-map environment form))
        (:phys-allocator-bootstrap
         (register-phys-allocator environment form))
        (:page-structures
         (register-page-structures environment form))
        (:syscall-table
         (register-syscall-table environment form))
        (:execution-slot-type
         (register-slot-type environment form))
        (:partition-table
         (register-partition-table environment form))
        (:volume
         (register-volume environment form))))))

(defstruct (routine-stack-descriptor
            (:constructor make-routine-stack-descriptor
                (&key name body initial-bits constants)))
  name
  body
  initial-bits
  constants)

(defstruct (stack-root-descriptor
            (:constructor make-stack-root-descriptor
                (&key routine stack-top-linear bits origin-form)))
  routine
  stack-top-linear
  bits
  origin-form)

(defun stack-analysis-resolve-operand (constants operand)
  (cond ((and (symbolp operand)
              (multiple-value-bind (_ present)
                  (gethash operand constants)
                (declare (ignore _))
                present))
         (stack-analysis-resolve-operand constants (gethash operand constants)))
        ((consp operand)
         (mapcar (lambda (item)
                   (stack-analysis-resolve-operand constants item))
                 operand))
        (t
         operand)))

(defun stack-analysis-word-bytes (bits)
  (ecase bits
    (16 2)
    (32 4)))

(defun stack-analysis-clamp-depth (depth)
  (max 0 depth))

(defun stack-analysis-terminal-form-p (form)
  (let ((op (token-keyword (first form))))
    (member op '(:hang :idle-forever :context-restore :privilege-drop :jump-section
                  :iret :ret :retf :lret :sysexit :sysret
                  :jmp :ljmp16 :ljmp32)
            :test #'eq)))

(defun stack-analysis-helper-call-form-p (form)
  (member (token-keyword (first form))
          '(:bios-print :debug-print :vga-print :vga-print-hex
            :vga-print-decimal :vga-print-ascii4 :serial-print
            :phys-alloc :phys-free)
          :test #'eq))

(defun stack-analysis-direct-call-target (constants operand)
  (let ((resolved (stack-analysis-resolve-operand constants operand)))
    (and (symbolp resolved) resolved)))

(defun collect-section-routines-for-stack-analysis (body)
  (let ((bits 16)
        (constants (make-hash-table :test 'eq))
        (routines '()))
    (labels ((record-const (name value)
               (setf (gethash name constants)
                     (stack-analysis-resolve-operand constants value)))
             (walk (forms)
               (dolist (form forms)
                 (when (consp form)
                   (case (token-keyword (first form))
                     (:const
                      (destructuring-bind (_ name value) form
                        (declare (ignore _))
                        (record-const name value)))
                     (:bits
                      (destructuring-bind (_ next-bits) form
                        (declare (ignore _))
                        (setf bits (stack-analysis-resolve-operand constants next-bits))))
                     (:repeat
                      (destructuring-bind (_ count &body repeated-body) form
                        (declare (ignore _))
                        (let ((count (stack-analysis-resolve-operand constants count)))
                          (unless (and (integerp count) (not (minusp count)))
                            (error "REPEAT requires a non-negative integer count, got ~S." count))
                          (loop repeat count do
                            (walk repeated-body)))))
                     (:routine
                      (destructuring-bind (_ name &body routine-body) form
                        (declare (ignore _))
                        (push (make-routine-stack-descriptor
                               :name name
                               :body routine-body
                               :initial-bits bits
                               :constants (copy-label-table constants))
                              routines)
                        ;; CONST/BITS inside routines persist in the section-wide
                        ;; compile environment, so mirror that behavior here.
                        (walk routine-body)))
                     (t nil))))))
      (walk body)
      (nreverse routines))))

(defun build-routine-stack-table (routines)
  (let ((table (make-hash-table :test 'eq)))
    (dolist (descriptor routines)
      (setf (gethash (routine-stack-descriptor-name descriptor) table)
            descriptor))
    table))

(defun infer-routine-initial-stack-root (section descriptor)
  (let ((bits (routine-stack-descriptor-initial-bits descriptor))
        (constants (copy-label-table (routine-stack-descriptor-constants descriptor))))
    (dolist (form (routine-stack-descriptor-body descriptor))
      (when (consp form)
        (case (token-keyword (first form))
          (:const
           (destructuring-bind (_ name value) form
             (declare (ignore _))
             (setf (gethash name constants)
                   (stack-analysis-resolve-operand constants value))))
          (:bits
           (destructuring-bind (_ next-bits) form
             (declare (ignore _))
             (setf bits (stack-analysis-resolve-operand constants next-bits))))
          (:label
           nil)
          (:initialize-real-mode
           (let* ((args (stack-analysis-resolve-operand constants (rest form)))
                  (segment (or (section-spec-load-segment section) 0))
                  (stack (getf args :stack #x7C00)))
             (return
               (make-stack-root-descriptor
                :routine (routine-stack-descriptor-name descriptor)
                :stack-top-linear (+ (* 16 segment) stack)
                :bits 16
                :origin-form 'initialize-real-mode))))
          (:initialize-protected-mode
           (let* ((args (stack-analysis-resolve-operand constants (rest form)))
                  (section-base (section-linear-base section))
                  (stack-linear (getf args :stack-linear))
                  (stack-top (if stack-linear
                                 stack-linear
                                 (+ section-base (getf args :stack #x8000)))))
             (return
               (make-stack-root-descriptor
                :routine (routine-stack-descriptor-name descriptor)
                :stack-top-linear stack-top
                :bits 32
                :origin-form 'initialize-protected-mode))))
          ;; Only trust the stack root when it is the first operational form.
          (t
           (return nil)))))))

(defun stack-root-capacity-bytes (target root)
  (let* ((top (stack-root-descriptor-stack-top-linear root))
         (region (find-if (lambda (candidate)
                            (and (eq (token-keyword (memory-region-type candidate)) :RAM)
                                 (<= (memory-region-start candidate)
                                     top
                                     (1+ (memory-region-end candidate)))))
                          (machine-descriptor-memory-regions target))))
    (and region
         (- top (memory-region-start region)))))

(defun compute-routine-stack-usage (descriptor routine-table cache visiting)
  (or (gethash (routine-stack-descriptor-name descriptor) cache)
      (progn
        (when (member (routine-stack-descriptor-name descriptor) visiting :test #'eq)
          (error "Recursive or cyclic call graph detected at routine ~S; stack usage is unbounded."
                 (routine-stack-descriptor-name descriptor)))
        (let ((usage
                (labels ((note-max (max depth)
                           (max max depth))
                         (call-usage (bits current constants target)
                           (let* ((width (stack-analysis-word-bytes bits))
                                  (callee (and target (gethash target routine-table))))
                             (+ current
                                width
                                (if callee
                                    (compute-routine-stack-usage callee
                                                                 routine-table
                                                                 cache
                                                                 (cons (routine-stack-descriptor-name descriptor)
                                                                       visiting))
                                    0))))
                         (walk-forms (forms current bits constants max-depth)
                           (dolist (form forms (values current bits constants max-depth nil))
                             (unless (consp form)
                               (error "Bootwright DSL forms must be lists, got ~S." form))
                             (multiple-value-bind (next-current next-bits next-constants next-max stop-p)
                                 (walk-form form current bits constants max-depth)
                               (setf current next-current
                                     bits next-bits
                                     constants next-constants
                                     max-depth next-max)
                               (when stop-p
                                 (return (values current bits constants max-depth t))))))
                         (walk-form (form current bits constants max-depth)
                           (case (token-keyword (first form))
                             (:const
                              (destructuring-bind (_ name value) form
                                (declare (ignore _))
                                (setf (gethash name constants)
                                      (stack-analysis-resolve-operand constants value))
                                (values current bits constants max-depth nil)))
                             (:bits
                              (destructuring-bind (_ next-bits) form
                                (declare (ignore _))
                                (values current
                                        (stack-analysis-resolve-operand constants next-bits)
                                        constants
                                        max-depth
                                        nil)))
                             (:repeat
                              (destructuring-bind (_ count &body repeated-body) form
                                (declare (ignore _))
                                (let ((count (stack-analysis-resolve-operand constants count))
                                      (stopped nil))
                                  (unless (and (integerp count) (not (minusp count)))
                                    (error "REPEAT requires a non-negative integer count, got ~S." count))
                                  (loop repeat count
                                        do (multiple-value-setq (current bits constants max-depth stopped)
                                             (walk-forms repeated-body current bits constants max-depth))
                                           (when stopped
                                             (return)))
                                  (values current bits constants max-depth stopped))))
                             (:forever
                              (destructuring-bind (_ &body loop-body) form
                                (declare (ignore _))
                                (multiple-value-bind (loop-current loop-bits loop-constants loop-max loop-stop)
                                    (walk-forms loop-body
                                                current
                                                bits
                                                (copy-label-table constants)
                                                max-depth)
                                  (declare (ignore loop-current loop-bits loop-constants loop-stop))
                                  (values current bits constants loop-max t))))
                             (:while-zero
                              (destructuring-bind (_ operand &body loop-body) form
                                (declare (ignore _ operand))
                                (multiple-value-bind (loop-current loop-bits loop-constants loop-max loop-stop)
                                    (walk-forms loop-body
                                                current
                                                bits
                                                (copy-label-table constants)
                                                max-depth)
                                  (declare (ignore loop-current loop-bits loop-constants loop-stop))
                                  (values current bits constants loop-max nil))))
                             (:while-nonzero
                              (destructuring-bind (_ operand &body loop-body) form
                                (declare (ignore _ operand))
                                (multiple-value-bind (loop-current loop-bits loop-constants loop-max loop-stop)
                                    (walk-forms loop-body
                                                current
                                                bits
                                                (copy-label-table constants)
                                                max-depth)
                                  (declare (ignore loop-current loop-bits loop-constants loop-stop))
                                  (values current bits constants loop-max nil))))
                             (:if-zero
                              (multiple-value-bind (_ then-body else-body)
                                  (parse-conditional-clauses form "IF-ZERO")
                                (declare (ignore _))
                                (multiple-value-bind (then-current then-bits then-constants then-max then-stop)
                                    (walk-forms then-body
                                                current
                                                bits
                                                (copy-label-table constants)
                                                max-depth)
                                  (declare (ignore then-current then-bits then-constants))
                                  (if else-body
                                      (multiple-value-bind (else-current else-bits else-constants else-max else-stop)
                                          (walk-forms else-body
                                                      current
                                                      bits
                                                      (copy-label-table constants)
                                                      then-max)
                                        (declare (ignore else-current else-bits else-constants))
                                        (values current
                                                bits
                                                constants
                                                else-max
                                                (and then-stop else-stop)))
                                      (values current bits constants then-max nil)))))
                             (:if-nonzero
                              (multiple-value-bind (_ then-body else-body)
                                  (parse-conditional-clauses form "IF-NONZERO")
                                (declare (ignore _))
                                (multiple-value-bind (then-current then-bits then-constants then-max then-stop)
                                    (walk-forms then-body
                                                current
                                                bits
                                                (copy-label-table constants)
                                                max-depth)
                                  (declare (ignore then-current then-bits then-constants))
                                  (if else-body
                                      (multiple-value-bind (else-current else-bits else-constants else-max else-stop)
                                          (walk-forms else-body
                                                      current
                                                      bits
                                                      (copy-label-table constants)
                                                      then-max)
                                        (declare (ignore else-current else-bits else-constants))
                                        (values current
                                                bits
                                                constants
                                                else-max
                                                (and then-stop else-stop)))
                                      (values current bits constants then-max nil)))))
                             (:routine
                              (values current bits constants max-depth nil))
                             (:irq-handler
                              (let* ((args (stack-analysis-resolve-operand constants (rest form)))
                                     (target (stack-analysis-direct-call-target constants (second args))))
                                (values current
                                        bits
                                        constants
                                        (note-max max-depth (call-usage bits current constants target))
                                        t)))
                             (:privilege-drop
                              (values current
                                      bits
                                      constants
                                      (note-max max-depth (+ current 20))
                                      t))
                             (t
                              (let ((op (token-keyword (first form))))
                                (case op
                                  (:push
                                   (let* ((depth (+ current (stack-analysis-word-bytes bits)))
                                          (next-max (note-max max-depth depth)))
                                     (values depth bits constants next-max nil)))
                                  (:pop
                                   (values (stack-analysis-clamp-depth
                                            (- current (stack-analysis-word-bytes bits)))
                                           bits constants max-depth nil))
                                  (:pushf
                                   (let* ((depth (+ current (stack-analysis-word-bytes bits)))
                                          (next-max (note-max max-depth depth)))
                                     (values depth bits constants next-max nil)))
                                  (:popf
                                   (values (stack-analysis-clamp-depth
                                            (- current (stack-analysis-word-bytes bits)))
                                           bits constants max-depth nil))
                                  (:pushfd
                                   (let* ((depth (+ current 4))
                                          (next-max (note-max max-depth depth)))
                                     (values depth bits constants next-max nil)))
                                  (:popfd
                                   (values (stack-analysis-clamp-depth (- current 4))
                                           bits constants max-depth nil))
                                  (:pusha
                                   (let* ((depth (+ current 16))
                                          (next-max (note-max max-depth depth)))
                                     (values depth bits constants next-max nil)))
                                  (:popa
                                   (values (stack-analysis-clamp-depth (- current 16))
                                           bits constants max-depth nil))
                                  (:pushad
                                   (let* ((depth (+ current 32))
                                          (next-max (note-max max-depth depth)))
                                     (values depth bits constants next-max nil)))
                                  (:popad
                                   (values (stack-analysis-clamp-depth (- current 32))
                                           bits constants max-depth nil))
                                  (:call
                                   (let* ((target (stack-analysis-direct-call-target
                                                   constants
                                                   (second form)))
                                          (next-max (note-max max-depth
                                                              (call-usage bits current constants target))))
                                     (values current bits constants next-max nil)))
                                  (otherwise
                                   (if (stack-analysis-helper-call-form-p form)
                                       (values current
                                               bits
                                               constants
                                               (note-max max-depth
                                                         (+ current (stack-analysis-word-bytes bits)))
                                               (stack-analysis-terminal-form-p form))
                                       (values current
                                               bits
                                               constants
                                               max-depth
                                               (stack-analysis-terminal-form-p form))))))))))
                  (nth-value 3
                             (walk-forms (routine-stack-descriptor-body descriptor)
                                         0
                                         (routine-stack-descriptor-initial-bits descriptor)
                                         (copy-label-table
                                          (routine-stack-descriptor-constants descriptor))
                                         0)))))
          (setf (gethash (routine-stack-descriptor-name descriptor) cache) usage)
          usage))))

(defun assert-section-stack-budget (section target)
  (let* ((routines (collect-section-routines-for-stack-analysis (section-spec-body section)))
         (routine-table (build-routine-stack-table routines))
         (entry (section-spec-entry section))
         (entry-routine (and entry (gethash entry routine-table))))
    (when entry-routine
      (let ((root (infer-routine-initial-stack-root section entry-routine)))
        (when root
          (let ((capacity (stack-root-capacity-bytes target root)))
            (when capacity
              (let ((usage (compute-routine-stack-usage entry-routine
                                                        routine-table
                                                        (make-hash-table :test 'eq)
                                                        '())))
                (when (> usage capacity)
                  (error "Routine ~S requires ~D bytes of stack, but ~S only provides ~D bytes."
                         (stack-root-descriptor-routine root)
                         usage
                         (stack-root-descriptor-origin-form root)
                         capacity))))))))))

(defun emit-keyboard-read (environment args)
  (require-bits environment 32 "KEYBOARD-READ")
  (let* ((state (compile-environment-state environment))
         (kbd-name (or (first args)
                       (error "KEYBOARD-READ requires a driver name.")))
         (dest (resolve-operand environment
                                (or (second args)
                                    (error "KEYBOARD-READ requires a destination byte register."))))
         (kbd (lookup-keyboard environment kbd-name))
         (mask (1- (keyboard-descriptor-buffer-size kbd)))
         (head (keyboard-descriptor-head-label kbd))
         (tail (keyboard-descriptor-tail-label kbd))
         (buffer (keyboard-descriptor-buffer-label kbd))
         (wait-label (gensym "KBD-WAIT-")))
    (unless (byte-register-p dest)
      (error "KEYBOARD-READ destination must be a byte register, got ~S." dest))
    (emit-label state wait-label)
    (encode-instruction state 'mov (list 'al (list :MEM :BYTE head)))
    (encode-instruction state 'cmp (list 'al (list :MEM :BYTE tail)))
    (encode-instruction state 'je (list wait-label))
    (encode-instruction state 'movzx '(ecx al))
    (encode-instruction state 'mov (list 'ebx buffer))
    (encode-instruction state 'mov (list dest (list :MEM :BYTE 'ebx 'ecx 1 0)))
    (encode-instruction state 'inc '(cl))
    (encode-instruction state 'and (list 'cl mask))
    (encode-instruction state 'mov (list (list :MEM :BYTE head) 'cl))))

(defun emit-keyboard-poll (environment args)
  (require-bits environment 32 "KEYBOARD-POLL")
  (let* ((state (compile-environment-state environment))
         (kbd-name (or (first args)
                       (error "KEYBOARD-POLL requires a driver name.")))
         (dest (resolve-operand environment
                                (or (second args)
                                    (error "KEYBOARD-POLL requires a destination byte register."))))
         (rest-args (resolve-operands environment (cddr args)))
         (empty-label (or (getf rest-args :empty)
                          (error "KEYBOARD-POLL requires an :EMPTY label.")))
         (kbd (lookup-keyboard environment kbd-name))
         (mask (1- (keyboard-descriptor-buffer-size kbd)))
         (head (keyboard-descriptor-head-label kbd))
         (tail (keyboard-descriptor-tail-label kbd))
         (buffer (keyboard-descriptor-buffer-label kbd)))
    (unless (byte-register-p dest)
      (error "KEYBOARD-POLL destination must be a byte register, got ~S." dest))
    (encode-instruction state 'mov (list 'al (list :MEM :BYTE head)))
    (encode-instruction state 'cmp (list 'al (list :MEM :BYTE tail)))
    (encode-instruction state 'je-near (list empty-label))
    (encode-instruction state 'movzx '(ecx al))
    (encode-instruction state 'mov (list 'ebx buffer))
    (encode-instruction state 'mov (list dest (list :MEM :BYTE 'ebx 'ecx 1 0)))
    (encode-instruction state 'inc '(cl))
    (encode-instruction state 'and (list 'cl mask))
    (encode-instruction state 'mov (list (list :MEM :BYTE head) 'cl))))

(defparameter +bootwright-floppy-sectors-per-track+ 18)
(defparameter +bootwright-floppy-heads+ 2)

(defun ensure-floppy-chs-range (lba)
  (let* ((sectors-per-cylinder (* +bootwright-floppy-sectors-per-track+
                                  +bootwright-floppy-heads+))
         (cylinder (floor lba sectors-per-cylinder)))
    (when (> cylinder 1023)
      (error "Section LBA ~D exceeds the BIOS CHS range Bootwright currently supports."
             lba))))

(defun lba-to-floppy-chs (lba)
  (ensure-floppy-chs-range lba)
  (let* ((sectors-per-cylinder (* +bootwright-floppy-sectors-per-track+
                                  +bootwright-floppy-heads+))
         (cylinder (floor lba sectors-per-cylinder))
         (cylinder-offset (mod lba sectors-per-cylinder))
         (head (floor cylinder-offset +bootwright-floppy-sectors-per-track+))
         (sector (1+ (mod cylinder-offset +bootwright-floppy-sectors-per-track+))))
    (values cylinder head sector)))

(defun emit-load-section (environment section-name args)
  (let* ((state (compile-environment-state environment))
         (layout (find-section-layout (compile-environment-layouts environment)
                                      section-name))
         (args (resolve-operands environment args))
         (error-label (getf args :on-error))
         (start-lba (section-layout-start-lba layout))
         (remaining (section-layout-sector-count layout))
         (load-segment (section-layout-load-segment layout))
         (load-offset (section-layout-load-offset layout))
         (linear-address (+ (* 16 load-segment) load-offset)))
    (unless (section-layout-load-segment layout)
      (error "Section ~S has no load segment and cannot be read by the BIOS loader."
             section-name))
    (loop with current-lba = start-lba
          with current-linear = linear-address
          while (> remaining 0)
          do (multiple-value-bind (cylinder head sector)
                 (lba-to-floppy-chs current-lba)
               (let* ((sector-offset (mod current-lba +bootwright-floppy-sectors-per-track+))
                      (track-space (- +bootwright-floppy-sectors-per-track+ sector-offset))
                      (segment (floor current-linear 16))
                      (offset (mod current-linear 16))
                      (bytes-to-boundary (- #x10000 offset))
                      (boundary-space (max 1 (floor bytes-to-boundary 512)))
                      (chunk (min remaining track-space boundary-space))
                      (cl-value (logior sector
                                        (ash (logand cylinder #x300) -2))))
                 (encode-instruction state 'mov (list 'ax segment))
                 (encode-instruction state 'mov '(es ax))
                 (encode-instruction state 'mov (list 'bx offset))
                 (encode-instruction state 'mov '(ah #x02))
                 (encode-instruction state 'mov (list 'al chunk))
                 (encode-instruction state 'mov (list 'ch (logand cylinder #xFF)))
                 (encode-instruction state 'mov (list 'cl cl-value))
                 (encode-instruction state 'mov (list 'dh head))
                 (encode-instruction state 'int '(#x13))
                 (when error-label
                   (encode-instruction state 'jc (list error-label)))
                 (decf remaining chunk)
                 (incf current-lba chunk)
                 (incf current-linear (* chunk 512)))))))

(defun emit-jump-section (environment section-name)
  (let ((layout (find-section-layout (compile-environment-layouts environment)
                                     section-name)))
    (unless (section-layout-load-segment layout)
      (error "Section ~S has no load segment and cannot be jumped to." section-name))
    (encode-instruction (compile-environment-state environment)
                        'ljmp16
                        (list (section-layout-entry-offset layout)
                              (section-layout-load-segment layout)))))

(defun compile-form (environment form)
  (unless (consp form)
    (error "Bootwright DSL forms must be lists, got ~S." form))
  (let ((state (compile-environment-state environment)))
    (case (token-keyword (first form))
      (:const
       (destructuring-bind (_ name value) form
         (declare (ignore _))
         (when (or (register-symbol-p name) (keywordp name))
           (error "CONST name ~S conflicts with an assembler token." name))
         (setf (gethash name (compile-environment-constants environment))
               (resolve-operand environment value))))
      (:label
       (destructuring-bind (_ name) form
         (declare (ignore _))
         (emit-label state name)))
      (:routine
       (destructuring-bind (_ name &body body) form
         (declare (ignore _))
         (emit-label state name)
         (dolist (nested-form body)
           (compile-form environment nested-form))))
      (:string
       (destructuring-bind (_ name value &rest args) form
         (declare (ignore _))
         (let* ((args (resolve-operands environment args))
                (zero-terminated (if (member :zero-terminated args)
                                     (getf args :zero-terminated)
                                     (getf args :z t)))
                (alignment (getf args :align)))
           (when alignment
             (emit-align state alignment))
           (emit-label state name)
           (if zero-terminated
               (emit-ascii-z state value)
               (append-octets (assembly-state-bytes state) (ascii-octets value))))))
      (:include-binary
       (emit-include-binary environment form))
      (:repeat
       (destructuring-bind (_ count &body body) form
         (declare (ignore _))
         (let ((count (resolve-operand environment count)))
           (unless (and (integerp count) (not (minusp count)))
             (error "REPEAT requires a non-negative integer count, got ~S." count))
           (loop repeat count do
             (dolist (nested-form body)
               (compile-form environment nested-form))))))
      (:forever
       (emit-forever-form environment form))
      (:while-zero
       (emit-while-zero-form environment form t))
      (:while-nonzero
       (emit-while-zero-form environment form nil))
      (:if-zero
       (emit-conditional-zero-form environment form t))
      (:if-nonzero
       (emit-conditional-zero-form environment form nil))
      (:db
       (emit-db state (resolve-operands environment (rest form))))
      (:dw
       (emit-dw state (resolve-operands environment (rest form))))
      (:dd
       (emit-dd state (resolve-operands environment (rest form))))
      (:resb
       (destructuring-bind (_ count) form
         (declare (ignore _))
         (loop repeat (resolve-operand environment count) do
           (emit-u8 state 0))))
      (:resw
       (destructuring-bind (_ count) form
         (declare (ignore _))
         (loop repeat (resolve-operand environment count) do
           (emit-u16 state 0))))
      (:resd
       (destructuring-bind (_ count) form
         (declare (ignore _))
         (loop repeat (resolve-operand environment count) do
           (emit-u32 state 0))))
      (:ascii
       (destructuring-bind (_ string) form
         (declare (ignore _))
         (append-octets (assembly-state-bytes state) (ascii-octets string))))
      (:ascii-z
       (destructuring-bind (_ string) form
         (declare (ignore _))
         (emit-ascii-z state string)))
      (:align
       (destructuring-bind (_ alignment &optional (fill 0)) form
         (declare (ignore _))
         (emit-align state
                     (resolve-operand environment alignment)
                     (resolve-operand environment fill))))
      (:pad-to
       (destructuring-bind (_ offset &optional (fill 0)) form
         (declare (ignore _))
         (emit-pad-to state
                      (resolve-operand environment offset)
                      (resolve-operand environment fill))))
      (:emit-helpers
       (emit-required-helpers environment))
      (:fill
       (destructuring-bind (_ count &optional (fill-byte 0)) form
         (declare (ignore _))
         (loop repeat (resolve-operand environment count) do
           (emit-u8 state (resolve-operand environment fill-byte)))))
      (:copy-memory
       (emit-copy-memory environment (rest form)))
      (:fill-memory
       (emit-fill-memory environment (rest form)))
      (:zero-memory
       (emit-zero-memory environment (rest form)))
      (:compare-memory
       (emit-compare-memory environment (rest form)))
      (:copy-string
       (emit-copy-string environment (rest form)))
      (:string-length
       (emit-string-length environment (rest form)))
      (:string-equal
       (emit-string-equal environment (rest form)))
      (:bits
       (destructuring-bind (_ bits) form
         (declare (ignore _))
         (set-assembly-bits state (resolve-operand environment bits))))
      (:arch
       (destructuring-bind (_ isa-name) form
         (declare (ignore _))
         (set-assembly-isa state (token-keyword isa-name))))
      (:gdt
       (emit-gdt environment form))
      (:idt
       (emit-idt environment form))
      (:page-structures
       (emit-page-structures environment form))
      (:page-table
       (emit-page-table environment form))
      (:page-directory
       (emit-page-directory environment form))
      (:boot-info
       (emit-boot-info environment form))
      (:bios-print
       (destructuring-bind (_ string-designator) form
         (declare (ignore _))
         (emit-bios-print environment (resolve-operand environment string-designator))))
      (:bios-print-at
       (destructuring-bind (_ string-designator &rest args) form
         (declare (ignore _))
         (emit-bios-print-at environment
                             (resolve-operand environment string-designator)
                             args)))
      (:debug-print
       (destructuring-bind (_ string-designator) form
         (declare (ignore _))
         (emit-debug-print environment (resolve-operand environment string-designator))))
      (:vga-print
       (destructuring-bind (_ string-designator &rest args) form
         (declare (ignore _))
         (emit-vga-print environment
                         (resolve-operand environment string-designator)
                         args)))
      (:vga-print-at
       (destructuring-bind (_ string-designator &rest args) form
         (declare (ignore _))
         (emit-vga-print-at environment
                            (resolve-operand environment string-designator)
                            args)))
      (:vga-box
       (emit-vga-box environment (rest form)))
      (:framebuffer-clear
       (destructuring-bind (_ framebuffer-designator &rest args) form
         (declare (ignore _))
         (emit-framebuffer-clear environment
                                 (resolve-operand environment framebuffer-designator)
                                 (resolve-operands environment args))))
      (:framebuffer-print-at
       (destructuring-bind (_ framebuffer-designator string-designator &rest args) form
         (declare (ignore _))
         (emit-framebuffer-print-at environment
                                    (resolve-operand environment framebuffer-designator)
                                    (resolve-operand environment string-designator)
                                    (resolve-operands environment args))))
      (:framebuffer-box
       (destructuring-bind (_ framebuffer-designator &rest args) form
         (declare (ignore _))
         (emit-framebuffer-box environment
                               (resolve-operand environment framebuffer-designator)
                               (resolve-operands environment args))))
      (:framebuffer-fill-region
       (destructuring-bind (_ framebuffer-designator &rest args) form
         (declare (ignore _))
         (emit-framebuffer-fill-region environment
                                       (resolve-operand environment framebuffer-designator)
                                       (resolve-operands environment args))))
      (:framebuffer-window
       (destructuring-bind (_ framebuffer-designator &rest args) form
         (declare (ignore _))
         (emit-framebuffer-window environment
                                  (resolve-operand environment framebuffer-designator)
                                  (resolve-operands environment args))))
      (:framebuffer-scroll
       (destructuring-bind (_ framebuffer-designator &rest args) form
         (declare (ignore _))
         (emit-framebuffer-scroll environment
                                  (resolve-operand environment framebuffer-designator)
                                  (resolve-operands environment args))))
      (:framebuffer-scroll-region
       (destructuring-bind (_ framebuffer-designator &rest args) form
         (declare (ignore _))
         (emit-framebuffer-scroll-region environment
                                         (resolve-operand environment framebuffer-designator)
                                         (resolve-operands environment args))))
      (:vga-print-hex
       (destructuring-bind (_ value-designator &rest args) form
         (declare (ignore _))
         (emit-vga-print-hex environment value-designator args)))
      (:vga-print-ascii4
       (destructuring-bind (_ value-designator &rest args) form
         (declare (ignore _))
         (emit-vga-print-ascii4 environment value-designator args)))
      (:vga-print-decimal
       (destructuring-bind (_ value-designator &rest args) form
         (declare (ignore _))
         (emit-vga-print-decimal environment value-designator args)))
      (:vga-set-cursor
       (emit-vga-set-cursor environment (rest form)))
      (:serial-init
       (emit-serial-init environment (rest form)))
      (:serial-print
       (destructuring-bind (_ string-designator) form
         (declare (ignore _))
         (emit-serial-print environment (resolve-operand environment string-designator))))
      (:uart-init
       (emit-uart-init environment (rest form)))
      (:uart-write-byte
       (emit-uart-write-byte environment (rest form)))
      (:uart-read-byte
       (emit-uart-read-byte environment (rest form)))
      (:uart-tx-ready-p
       (emit-uart-tx-ready-p environment (rest form)))
      (:uart-print
       (emit-uart-print environment (rest form)))
      (:pc-speaker-tone
       (emit-pc-speaker-tone environment (rest form)))
      (:pc-speaker-off
       (emit-pc-speaker-off environment))
      (:initialize-real-mode
       (emit-real-mode-prologue environment (rest form)))
      (:initialize-protected-mode
       (emit-protected-mode-prologue environment (rest form)))
      (:enter-protected-mode
       (emit-enter-protected-mode environment (rest form)))
      (:load-gdt
       (emit-load-gdt environment (rest form)))
      (:set-video-mode
       (emit-set-video-mode environment (rest form)))
      (:bios-set-cursor
       (emit-bios-set-cursor environment (rest form)))
      (:enable-a20-fast
       (emit-enable-a20-fast environment))
      (:io-wait
       (emit-io-wait environment))
      (:clear-vga
       (emit-clear-vga environment (rest form)))
      (:hang
       (emit-hang environment))
      (:idle-forever
       (emit-idle-forever environment))
      (:enable-paging
       (emit-enable-paging environment (rest form)))
      (:load-idt
       (emit-load-idt environment (rest form)))
      (:memory-barrier
       (emit-memory-barrier environment (rest form)))
      (:tlb-flush-page
       (emit-tlb-flush-page environment (rest form)))
      (:tlb-flush-all
       (emit-tlb-flush-all environment))
      (:load-tr
       (emit-load-tr environment (rest form)))
      (:pic-remap
       (emit-pic-remap environment (rest form)))
      (:pic-mask-all
       (emit-pic-mask-all environment (rest form)))
      (:pic-eoi
       (emit-pic-eoi environment (rest form)))
      (:pic-mask
       (emit-pic-mask-operation environment (rest form) t))
      (:pic-unmask
       (emit-pic-mask-operation environment (rest form) nil))
      (:pit-set-frequency
       (emit-pit-set-frequency environment (rest form)))
      (:irq-remap
       (emit-irq-remap environment (rest form)))
      (:irq-mask
       (emit-irq-mask environment (rest form)))
      (:irq-unmask
       (emit-irq-unmask environment (rest form)))
      (:irq-end-of-interrupt
       (emit-irq-end-of-interrupt environment (rest form)))
      (:irq-handler
       (emit-irq-handler environment (rest form)))
      (:timer-set-frequency
       (emit-timer-set-frequency environment (rest form)))
      (:timer-enable
       (emit-timer-enable environment))
      (:timer-disable
       (emit-timer-disable environment))
      (:timer-read-counter
       (emit-timer-read-counter environment (rest form)))
      (:keyboard-driver
       (emit-keyboard-driver environment form))
      (:keyboard-read
       (emit-keyboard-read environment (rest form)))
      (:keyboard-poll
       (emit-keyboard-poll environment (rest form)))
      (:block-device
       ;; Pre-scan already registered the descriptor; no code/data emitted.
       nil)
      (:block-read
       (emit-block-read environment (rest form)))
      (:block-write
       (emit-block-write environment (rest form)))
      (:partition-table
       (emit-partition-table environment form))
      (:partition-table-read
       (emit-partition-table-read environment (rest form)))
      (:partition-find
       (emit-partition-find environment (rest form)))
      (:volume
       (emit-volume environment form))
      (:volume-bind
       (emit-volume-bind environment (rest form)))
      (:volume-read
       (emit-volume-read environment (rest form)))
      (:volume-write
       (emit-volume-write environment (rest form)))
      (:memory-map
       (emit-memory-map environment form))
      (:memory-map-probe
       (emit-memory-map-probe environment (rest form)))
      (:memory-map-base
       (emit-memory-map-base environment (rest form)))
      (:memory-map-count
       (emit-memory-map-count environment (rest form)))
      (:memory-map-iterate
       (emit-memory-map-iterate environment (rest form)))
      (:phys-allocator-bootstrap
       (emit-phys-allocator-bootstrap environment form))
      (:phys-allocator-init
       (emit-phys-allocator-init environment (rest form)))
      (:phys-alloc
       (emit-phys-alloc environment (rest form)))
      (:phys-free
       (emit-phys-free environment (rest form)))
      (:map-page
       (emit-map-page environment (rest form)))
      (:unmap-page
       (emit-unmap-page environment (rest form)))
      (:execution-slot-type
       (emit-execution-slot-type environment form))
      (:execution-slot
       (emit-execution-slot environment form))
      (:context-save
       (emit-context-save environment (rest form)))
      (:context-restore
       (emit-context-restore environment (rest form)))
      (:context-init
       (emit-context-init environment (rest form)))
      (:syscall-table
       (emit-syscall-table environment form))
      (:tss
       (emit-tss environment form))
      (:privilege-drop
       (emit-privilege-drop environment (rest form)))
      (:sysenter-setup
       (emit-sysenter-setup environment (rest form)))
      (:syscall-setup
       (emit-syscall-setup environment (rest form)))
      (:load-section
       (destructuring-bind (_ section-name &rest args) form
         (declare (ignore _))
         (emit-load-section environment section-name args)))
      (:jump-section
       (destructuring-bind (_ section-name) form
         (declare (ignore _))
         (emit-jump-section environment section-name)))
      (t
       (let ((personality-form (lookup-active-personality-form (first form))))
         (if personality-form
             (funcall personality-form environment form)
             (encode-instruction state
                                 (first form)
                                 (resolve-operands environment (rest form)))))))))

(defun compile-section-spec (section target layouts)
  (let* ((origin (section-origin section))
         (linear-base (section-linear-base section))
         (state (make-assembly-state :origin origin
                                     :linear-base linear-base
                                     :bits 16))
         (environment (make-compile-environment :target target
        :section section
        :layouts layouts
        :state state
        :helpers (make-hash-table :test 'eq)
        :constants (make-hash-table :test 'eq)
        :keyboards (make-hash-table :test 'eq)
        :block-devices (make-hash-table :test 'eq)
        :memory-maps (make-hash-table :test 'eq)
        :phys-allocators (make-hash-table :test 'eq)
        :page-structures (make-hash-table :test 'eq)
        :syscall-tables (make-hash-table :test 'eq)
        :slot-types (make-hash-table :test 'eq)
        :partition-tables (make-hash-table :test 'eq)
        :volumes (make-hash-table :test 'eq))))
    (pre-scan-section environment (section-spec-body section))
    (assert-section-stack-budget section target)
    (dolist (form (section-spec-body section))
      (compile-form environment form))
    (emit-required-helpers environment)
    (multiple-value-bind (bytes labels) (finalize-assembly state)
      (let ((entry-address (and (section-spec-entry section)
                                (gethash (section-spec-entry section) labels))))
        (when (and (section-spec-entry section) (null entry-address))
          (error "Section ~S declares missing entry label ~S."
                 (section-spec-name section)
                 (section-spec-entry section)))
        (make-compiled-section :spec section
                               :bytes bytes
                               :origin origin
                               :labels labels
                               :entry-address entry-address)))))

(defmacro defimage (name options &body sections)
  (let ((target (or (getf options :machine)
                    (getf options :target))))
    (unless target
      (error "DEFIMAGE requires a :MACHINE or :TARGET option."))
    `(defparameter ,name
       (make-image-spec :name ',name
                        :target ',target
                        :sections ',sections
                        :personalities (take-pending-personalities)
                        :source-pathname ,(or *load-truename*
                                              *load-pathname*
                                              *compile-file-truename*)))))
