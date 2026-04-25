(in-package #:bootwright)

(defstruct (compile-environment
            (:constructor make-compile-environment
                (&key target section layouts state helpers constants)))
  target
  section
  layouts
  state
  helpers
  constants)

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
             (emit-serial-print-helper environment))))))))

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
         (channel (getf (rest args) :channel 0))
         (channel-port (ecase channel
                         (0 (pit8253-timer-channel0-port device))
                         (1 (pit8253-timer-channel1-port device))
                         (2 (pit8253-timer-channel2-port device))))
         (latch-command (ash channel 6)))
    (unless (typep channel '(integer 0 2))
      (error "PIT channel must be 0, 1, or 2, got ~S." channel))
    (unless (or (word-register-p destination)
                (dword-register-p destination))
      (error "TIMER-READ-COUNTER requires a 16-bit or 32-bit destination register, got ~S."
             destination))
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
           (encode-instruction state 'movzx (list destination 'ax))))))

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

(defun ensure-floppy-track-range (layout)
  (let ((last-sector (+ (section-layout-start-lba layout)
                        (section-layout-sector-count layout))))
    (when (> last-sector 18)
      (error "Section ~S exceeds the single-track BIOS loader used by Bootwright."
             (section-layout-name layout)))))

(defun emit-load-section (environment section-name args)
  (let* ((state (compile-environment-state environment))
         (layout (find-section-layout (compile-environment-layouts environment)
                                      section-name))
         (args (resolve-operands environment args))
         (error-label (getf args :on-error)))
    (ensure-floppy-track-range layout)
    (unless (section-layout-load-segment layout)
      (error "Section ~S has no load segment and cannot be read by the BIOS loader."
             section-name))
    (encode-instruction state 'mov (list 'ax (section-layout-load-segment layout)))
    (encode-instruction state 'mov '(es ax))
    (encode-instruction state 'mov (list 'bx (section-layout-load-offset layout)))
    (encode-instruction state 'mov '(ah #x02))
    (encode-instruction state 'mov (list 'al (section-layout-sector-count layout)))
    (encode-instruction state 'mov '(ch 0))
    (encode-instruction state 'mov (list 'cl (1+ (section-layout-start-lba layout))))
    (encode-instruction state 'mov '(dh 0))
    (encode-instruction state 'int '(#x13))
    (when error-label
      (encode-instruction state 'jc (list error-label)))))

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
      (:db
       (emit-db state (resolve-operands environment (rest form))))
      (:dw
       (emit-dw state (resolve-operands environment (rest form))))
      (:dd
       (emit-dd state (resolve-operands environment (rest form))))
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
      (:fill
       (destructuring-bind (_ count &optional (fill-byte 0)) form
         (declare (ignore _))
         (loop repeat (resolve-operand environment count) do
           (emit-u8 state (resolve-operand environment fill-byte)))))
      (:bits
       (destructuring-bind (_ bits) form
         (declare (ignore _))
         (set-assembly-bits state (resolve-operand environment bits))))
      (:gdt
       (emit-gdt environment form))
      (:idt
       (emit-idt environment form))
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
      (:timer-set-frequency
       (emit-timer-set-frequency environment (rest form)))
      (:timer-enable
       (emit-timer-enable environment))
      (:timer-disable
       (emit-timer-disable environment))
      (:timer-read-counter
       (emit-timer-read-counter environment (rest form)))
      (:load-section
       (destructuring-bind (_ section-name &rest args) form
         (declare (ignore _))
         (emit-load-section environment section-name args)))
      (:jump-section
       (destructuring-bind (_ section-name) form
         (declare (ignore _))
         (emit-jump-section environment section-name)))
      (t
       (encode-instruction state
                           (first form)
                           (resolve-operands environment (rest form)))))))

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
        :constants (make-hash-table :test 'eq))))
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
                        :source-pathname ,(or *load-truename*
                                              *load-pathname*
                                              *compile-file-truename*)))))
