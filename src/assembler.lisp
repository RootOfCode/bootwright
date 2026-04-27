(in-package #:bootwright)

(defparameter +x86-byte-registers+
  '(("AL" . 0) ("CL" . 1) ("DL" . 2) ("BL" . 3)
    ("AH" . 4) ("CH" . 5) ("DH" . 6) ("BH" . 7)))

(defparameter +x86-word-registers+
  '(("AX" . 0) ("CX" . 1) ("DX" . 2) ("BX" . 3)
    ("SP" . 4) ("BP" . 5) ("SI" . 6) ("DI" . 7)))

(defparameter +x86-dword-registers+
  '(("EAX" . 0) ("ECX" . 1) ("EDX" . 2) ("EBX" . 3)
    ("ESP" . 4) ("EBP" . 5) ("ESI" . 6) ("EDI" . 7)))

(defparameter +x86-segment-registers+
  '(("ES" . 0) ("CS" . 1) ("SS" . 2) ("DS" . 3)
    ("FS" . 4) ("GS" . 5)))

(defparameter +x86-control-registers+
  '(("CR0" . 0) ("CR2" . 2) ("CR3" . 3) ("CR4" . 4)))

(defstruct (fixup (:constructor make-fixup (&key kind position target (addend 0))))
  kind
  position
  target
  addend)

(defstruct (assembly-state
            (:constructor %make-assembly-state
                (&key origin linear-base default-bits bytes labels fixups)))
  origin
  linear-base
  default-bits
  bytes
  labels
  fixups)

(defun make-assembly-state (&key (origin 0) (linear-base origin) (bits 16))
  (%make-assembly-state :origin origin
                        :linear-base linear-base
                        :default-bits bits
                        :bytes (make-octet-buffer)
                        :labels (make-hash-table :test 'equal)
                        :fixups '()))

(defun assembly-offset (state)
  (length (assembly-state-bytes state)))

(defun assembly-address (state)
  (+ (assembly-state-origin state) (assembly-offset state)))

(defun assembly-linear-address (state)
  (+ (assembly-state-linear-base state) (assembly-offset state)))

(defun assembly-bits (state)
  (assembly-state-default-bits state))

(defun set-assembly-bits (state bits)
  (unless (member bits '(16 32))
    (error "Bootwright only supports 16-bit and 32-bit code generation, got ~S." bits))
  (setf (assembly-state-default-bits state) bits)
  state)

(defun emit-u8 (state value)
  (append-octet (assembly-state-bytes state) value))

(defun emit-u16 (state value)
  (let ((word (ensure-word value)))
    (emit-u8 state (ldb (byte 8 0) word))
    (emit-u8 state (ldb (byte 8 8) word))))

(defun emit-u32 (state value)
  (let ((dword (ensure-dword value)))
    (emit-u8 state (ldb (byte 8 0) dword))
    (emit-u8 state (ldb (byte 8 8) dword))
    (emit-u8 state (ldb (byte 8 16) dword))
    (emit-u8 state (ldb (byte 8 24) dword))))

(defun patch-u8 (buffer position value)
  (setf (aref buffer position) (ensure-octet value)))

(defun patch-u16 (buffer position value)
  (let ((word (ensure-word value)))
    (setf (aref buffer position) (ldb (byte 8 0) word)
          (aref buffer (1+ position)) (ldb (byte 8 8) word))))

(defun patch-u32 (buffer position value)
  (let ((dword (ensure-dword value)))
    (setf (aref buffer position) (ldb (byte 8 0) dword)
          (aref buffer (+ position 1)) (ldb (byte 8 8) dword)
          (aref buffer (+ position 2)) (ldb (byte 8 16) dword)
          (aref buffer (+ position 3)) (ldb (byte 8 24) dword))))

(defun emit-label (state name)
  (setf (gethash name (assembly-state-labels state)) (assembly-address state)))

(defun register-code (operand register-class)
  (when (typep operand '(or symbol string))
    (cdr (assoc (canonical-token operand) register-class :test #'string=))))

(defun byte-register-p (operand)
  (register-code operand +x86-byte-registers+))

(defun word-register-p (operand)
  (register-code operand +x86-word-registers+))

(defun dword-register-p (operand)
  (register-code operand +x86-dword-registers+))

(defun segment-register-p (operand)
  (register-code operand +x86-segment-registers+))

(defun control-register-p (operand)
  (register-code operand +x86-control-registers+))

(defun register-symbol-p (operand)
  (or (byte-register-p operand)
      (word-register-p operand)
      (dword-register-p operand)
      (segment-register-p operand)
      (control-register-p operand)))

(defun operand-width (operand)
  (cond ((byte-register-p operand) 8)
        ((word-register-p operand) 16)
        ((dword-register-p operand) 32)
        (t nil)))

(defun memory-width-designator-p (operand)
  (and (typep operand '(or symbol string))
       (case (token-keyword operand)
         (:byte 8)
         (:word 16)
         (:dword 32)
         (t nil))))

(defun memory-operand-p (operand)
  (and (consp operand)
       (eq (token-keyword (first operand)) :MEM)))

(defun memory-operand-width (operand)
  (when (memory-operand-p operand)
    (memory-width-designator-p (second operand))))

(defun parse-memory-operand (operand)
  (unless (memory-operand-p operand)
    (error "Expected a memory operand, got ~S." operand))
  (let* ((parts (rest operand))
         (width (memory-width-designator-p (first parts))))
    (when width
      (setf parts (rest parts)))
    (unless parts
      (error "Memory operand ~S is missing an address." operand))
    (values width parts)))

(defun label-reference-p (operand)
  (and (consp operand)
       (eq (first operand) :label)
       (= (length operand) 2)))

(defun linear-reference-p (operand)
  (and (consp operand)
       (eq (first operand) :linear)
       (= (length operand) 2)))

(defun linear-addend-reference-p (operand)
  (and (consp operand)
       (eq (first operand) :linear+)
       (= (length operand) 3)))

(defun low16-reference-p (operand)
  (and (consp operand)
       (eq (first operand) :low16)
       (= (length operand) 2)))

(defun high16-reference-p (operand)
  (and (consp operand)
       (eq (first operand) :high16)
       (= (length operand) 2)))

(defun normalize-immediate (operand)
  (cond ((or (integerp operand)
             (label-reference-p operand)
             (linear-reference-p operand)
             (linear-addend-reference-p operand)
             (low16-reference-p operand)
             (high16-reference-p operand))
         operand)
        ((and (consp operand)
              (eq (token-keyword (first operand)) :linear-address)
              (= (length operand) 2))
         (list :linear (second operand)))
        ((and (symbolp operand) (not (register-symbol-p operand)))
         (list :label operand))
        (t
         operand)))

(defun emit-signed-u8 (state value)
  (unless (typep value '(integer -128 255))
    (error "Value ~S does not fit in an 8-bit signed/unsigned field." value))
  (emit-u8 state (ldb (byte 8 0) value)))

(defun emit-signed-u32 (state value)
  (unless (typep value '(integer -2147483648 4294967295))
    (error "Value ~S does not fit in a 32-bit signed/unsigned field." value))
  (emit-u32 state (ldb (byte 32 0) value)))

(defun emit-modrm (state mod reg rm)
  (emit-u8 state (logior (ash mod 6)
                         (ash reg 3)
                         rm)))

(defun emit-sib (state scale index base)
  (let ((scale-bits (ecase scale
                      (1 0)
                      (2 1)
                      (4 2)
                      (8 3))))
    (emit-u8 state (logior (ash scale-bits 6)
                           (ash index 3)
                           base))))

(defun add-fixup (state kind target position &optional (addend 0))
  (push (make-fixup :kind kind :position position :target target :addend addend)
        (assembly-state-fixups state)))

(defun emit-operand-size-prefix (state width)
  (when (and (/= width 8)
             (/= width (assembly-bits state)))
    (emit-u8 state #x66)))

(defun emit-imm8 (state operand)
  (let ((normalized (normalize-immediate operand))
        (position (assembly-offset state)))
    (cond ((integerp normalized)
           (emit-u8 state normalized))
          ((label-reference-p normalized)
           (emit-u8 state 0)
           (add-fixup state :imm8 (second normalized) position))
          (t
           (error "Unsupported 8-bit immediate operand ~S." operand)))))

(defun emit-imm16 (state operand)
  (let ((normalized (normalize-immediate operand))
        (position (assembly-offset state)))
    (cond ((integerp normalized)
           (emit-u16 state normalized))
          ((label-reference-p normalized)
           (emit-u16 state 0)
           (add-fixup state :imm16 (second normalized) position))
          ((linear-reference-p normalized)
           (emit-u16 state 0)
           (add-fixup state :imm16-linear (second normalized) position))
          ((low16-reference-p normalized)
           (emit-u16 state 0)
           (add-fixup state :imm16-low (second normalized) position))
          ((high16-reference-p normalized)
           (emit-u16 state 0)
           (add-fixup state :imm16-high (second normalized) position))
          (t
           (error "Unsupported 16-bit immediate operand ~S." operand)))))

(defun emit-imm32 (state operand)
  (let ((normalized (normalize-immediate operand))
        (position (assembly-offset state)))
    (cond ((integerp normalized)
           (emit-u32 state normalized))
          ((label-reference-p normalized)
           (emit-u32 state 0)
           (add-fixup state :imm32 (second normalized) position))
          ((linear-reference-p normalized)
           (emit-u32 state 0)
           (add-fixup state :imm32-linear (second normalized) position))
          ((linear-addend-reference-p normalized)
           (emit-u32 state 0)
           (add-fixup state :imm32-linear (second normalized) position (third normalized)))
          (t
           (error "Unsupported 32-bit immediate operand ~S." operand)))))

(defun emit-rel8 (state label)
  (let ((target (normalize-immediate label))
        (position (assembly-offset state)))
    (unless (label-reference-p target)
      (error "Relative 8-bit control flow requires a label, got ~S." label))
    (emit-u8 state 0)
    (add-fixup state :rel8 (second target) position)))

(defun emit-rel16 (state label)
  (let ((target (normalize-immediate label))
        (position (assembly-offset state)))
    (unless (label-reference-p target)
      (error "Relative 16-bit control flow requires a label, got ~S." label))
    (emit-u16 state 0)
    (add-fixup state :rel16 (second target) position)))

(defun emit-rel32 (state label)
  (let ((target (normalize-immediate label))
        (position (assembly-offset state)))
    (unless (label-reference-p target)
      (error "Relative 32-bit control flow requires a label, got ~S." label))
    (emit-u32 state 0)
    (add-fixup state :rel32 (second target) position)))

(defun resolve-fixups (state)
  (let ((buffer (assembly-state-bytes state))
        (labels (assembly-state-labels state))
        (origin (assembly-state-origin state))
        (linear-base (assembly-state-linear-base state)))
    (dolist (fixup (reverse (assembly-state-fixups state)))
      (let* ((target-address (gethash (fixup-target fixup) labels))
             (position (fixup-position fixup))
             (kind (fixup-kind fixup))
             (addend (fixup-addend fixup)))
        (unless target-address
          (error "Unknown label ~S." (fixup-target fixup)))
        (let ((offset-target (+ target-address addend))
              (linear-target (+ linear-base (- target-address origin) addend)))
          (ecase kind
            (:imm8
             (patch-u8 buffer position offset-target))
            (:imm16
             (patch-u16 buffer position offset-target))
            (:imm16-linear
             (patch-u16 buffer position linear-target))
            (:imm16-low
             (patch-u16 buffer position (ldb (byte 16 0) offset-target)))
            (:imm16-high
             (patch-u16 buffer position (ldb (byte 16 16) offset-target)))
            (:imm32
             (patch-u32 buffer position offset-target))
            (:imm32-linear
             (patch-u32 buffer position linear-target))
            (:rel8
             (let ((delta (- target-address (+ origin position 1))))
               (unless (typep delta '(integer -128 127))
                 (error "Relative 8-bit jump to ~S is out of range: ~D."
                        (fixup-target fixup) delta))
               (patch-u8 buffer position (ldb (byte 8 0) delta))))
            (:rel16
             (let ((delta (- target-address (+ origin position 2))))
               (unless (typep delta '(integer -32768 32767))
                 (error "Relative 16-bit jump to ~S is out of range: ~D."
                        (fixup-target fixup) delta))
               (patch-u16 buffer position (ldb (byte 16 0) delta))))
            (:rel32
             (let ((delta (- target-address (+ origin position 4))))
               (unless (typep delta '(integer -2147483648 2147483647))
                 (error "Relative 32-bit jump to ~S is out of range: ~D."
                        (fixup-target fixup) delta))
               (patch-u32 buffer position (ldb (byte 32 0) delta))))))))
    state))

(defun finalize-assembly (state)
  (resolve-fixups state)
  (values (assembly-state-bytes state)
          (copy-label-table (assembly-state-labels state))))

(defun emit-register-immediate (state base register operand width)
  (emit-operand-size-prefix state width)
  (emit-u8 state (+ base register))
  (ecase width
    (8 (emit-imm8 state operand))
    (16 (emit-imm16 state operand))
    (32 (emit-imm32 state operand))))

(defun emit-register-register (state opcode destination source destination-class source-class width)
  (let ((dst (register-code destination destination-class))
        (src (register-code source source-class)))
    (unless (and dst src)
      (error "Cannot encode ~S -> ~S with opcode #x~2,'0X." source destination opcode))
    (emit-operand-size-prefix state width)
    (emit-u8 state opcode)
    (emit-modrm state 3 src dst)))

(defun ensure-memory-width (operand width)
  (let ((declared-width (memory-operand-width operand)))
    (when (and declared-width (/= declared-width width))
      (error "Memory operand ~S declares width ~D, but the instruction requires ~D."
             operand declared-width width))
    width))

(defun emit-memory-reference-32 (state reg-field operand)
  (multiple-value-bind (_ parts)
      (parse-memory-operand operand)
    (declare (ignore _))
    (flet ((base-register-code (designator)
             (or (dword-register-p designator)
                 (error "32-bit memory addressing requires a 32-bit base register, got ~S."
                        designator))))
      (case (length parts)
        (1
         (let ((part (first parts)))
           (if (dword-register-p part)
               (let ((base (base-register-code part)))
                 (cond ((= base 4)
                        (emit-modrm state 0 reg-field 4)
                        (emit-sib state 1 4 4))
                       ((= base 5)
                        (emit-modrm state 1 reg-field 5)
                        (emit-signed-u8 state 0))
                       (t
                        (emit-modrm state 0 reg-field base))))
               (progn
                 (emit-modrm state 0 reg-field 5)
                 (emit-imm32 state part)))))
        ((2 4)
         (let ((base (first parts))
               (displacement (if (= (length parts) 2)
                                 (second parts)
                                 (fourth parts))))
           (unless (integerp displacement)
             (error "Memory displacement must be an integer, got ~S." displacement))
           (if (= (length parts) 2)
               (let ((base-code (base-register-code base)))
                 (cond ((and (/= base-code 5)
                             (typep displacement '(integer -128 127))
                             (/= displacement 0))
                        (if (= base-code 4)
                            (progn
                              (emit-modrm state 1 reg-field 4)
                              (emit-sib state 1 4 4))
                            (emit-modrm state 1 reg-field base-code))
                        (emit-signed-u8 state displacement))
                       ((and (/= base-code 5) (= displacement 0))
                        (if (= base-code 4)
                            (progn
                              (emit-modrm state 0 reg-field 4)
                              (emit-sib state 1 4 4))
                            (emit-modrm state 0 reg-field base-code)))
                       (t
                        (if (= base-code 4)
                            (progn
                              (emit-modrm state 2 reg-field 4)
                              (emit-sib state 1 4 4))
                            (emit-modrm state 2 reg-field base-code))
                        (emit-signed-u32 state displacement))))
               (destructuring-bind (base index scale _disp) parts
                 (declare (ignore _disp))
                 (let ((base-code (base-register-code base))
                       (index-code (base-register-code index)))
                   (when (= index-code 4)
                     (error "ESP cannot be used as a scaled index in ~S." operand))
                   (cond ((and (/= base-code 5)
                               (typep displacement '(integer -128 127))
                               (/= displacement 0))
                          (emit-modrm state 1 reg-field 4)
                          (emit-sib state scale index-code base-code)
                          (emit-signed-u8 state displacement))
                         ((and (/= base-code 5) (= displacement 0))
                          (emit-modrm state 0 reg-field 4)
                          (emit-sib state scale index-code base-code))
                         (t
                          (emit-modrm state (if (typep displacement '(integer -128 127)) 1 2)
                                      reg-field
                                      4)
                          (emit-sib state scale index-code base-code)
                          (if (typep displacement '(integer -128 127))
                              (emit-signed-u8 state displacement)
                              (emit-signed-u32 state displacement)))))))))
        (t
         (error "Unsupported memory operand syntax ~S." operand))))))

(defun emit-modrm-r/m (state reg-field operand)
  (cond ((dword-register-p operand)
         (emit-modrm state 3 reg-field (dword-register-p operand)))
        ((word-register-p operand)
         (emit-modrm state 3 reg-field (word-register-p operand)))
        ((byte-register-p operand)
         (emit-modrm state 3 reg-field (byte-register-p operand)))
        ((memory-operand-p operand)
         (ecase (assembly-bits state)
           (16
            (multiple-value-bind (_ parts)
                (parse-memory-operand operand)
              (declare (ignore _))
              (unless (= (length parts) 1)
                (error "16-bit memory operands currently support direct addresses only, got ~S."
                       operand))
              (emit-modrm state 0 reg-field 6)
              (emit-imm16 state (first parts))))
           (32
            (emit-memory-reference-32 state reg-field operand))))
        (t
         (error "Unsupported r/m operand ~S." operand))))

(defun emit-r/m-register (state opcode r/m-operand register width register-class)
  (let ((reg (register-code register register-class)))
    (unless reg
      (error "Unsupported register operand ~S." register))
    (when (memory-operand-p r/m-operand)
      (ensure-memory-width r/m-operand width))
    (emit-operand-size-prefix state width)
    (emit-u8 state opcode)
    (emit-modrm-r/m state reg r/m-operand)))

(defun emit-register-r/m (state opcode register r/m-operand width register-class)
  (let ((reg (register-code register register-class)))
    (unless reg
      (error "Unsupported register operand ~S." register))
    (when (memory-operand-p r/m-operand)
      (ensure-memory-width r/m-operand width))
    (emit-operand-size-prefix state width)
    (emit-u8 state opcode)
    (emit-modrm-r/m state reg r/m-operand)))

(defun emit-group1-immediate (state opcode-ext operand immediate width)
  (when (memory-operand-p operand)
    (ensure-memory-width operand width))
  (ecase width
    (8
     (emit-u8 state #x80)
     (emit-modrm-r/m state opcode-ext operand)
     (emit-imm8 state immediate))
    (16
     (emit-operand-size-prefix state 16)
     (emit-u8 state #x81)
     (emit-modrm-r/m state opcode-ext operand)
     (emit-imm16 state immediate))
    (32
     (emit-operand-size-prefix state 32)
     (emit-u8 state #x81)
     (emit-modrm-r/m state opcode-ext operand)
     (emit-imm32 state immediate))))

(defun emit-unary-register-op (state base operand width register-class)
  (let ((reg (register-code operand register-class)))
    (unless reg
      (error "Unsupported register operand ~S." operand))
    (emit-operand-size-prefix state width)
    (emit-u8 state (+ base reg))))

(defun emit-far-jump (state offset selector width)
  (emit-operand-size-prefix state width)
  (emit-u8 state #xEA)
  (ecase width
    (16 (emit-imm16 state offset))
    (32 (emit-imm32 state offset)))
  (emit-imm16 state selector))

(defun emit-absolute-memory-reference (state reg-field operand)
  (ecase (assembly-bits state)
    (16
     (emit-modrm state 0 reg-field 6)
     (emit-imm16 state operand))
    (32
     (emit-modrm state 0 reg-field 5)
     (emit-imm32 state operand))))

(defun emit-unary-rm-op (state opcode-ext operand)
  ;; F6 / F7 with /opcode-ext targeting an 8/16/32-bit register.
  ;; Used by NOT, NEG, MUL, DIV.
  (cond ((dword-register-p operand)
         (emit-operand-size-prefix state 32)
         (emit-u8 state #xF7)
         (emit-modrm-r/m state opcode-ext operand))
        ((word-register-p operand)
         (emit-operand-size-prefix state 16)
         (emit-u8 state #xF7)
         (emit-modrm-r/m state opcode-ext operand))
        ((byte-register-p operand)
         (emit-u8 state #xF6)
         (emit-modrm-r/m state opcode-ext operand))
        ((memory-operand-p operand)
         (let ((width (or (memory-operand-width operand)
                          (error "Unary memory operands require an explicit width, got ~S."
                                 operand))))
           (ecase width
             (8
              (emit-u8 state #xF6)
              (emit-modrm-r/m state opcode-ext operand))
             (16
              (emit-operand-size-prefix state 16)
              (emit-u8 state #xF7)
              (emit-modrm-r/m state opcode-ext operand))
             (32
              (emit-operand-size-prefix state 32)
              (emit-u8 state #xF7)
              (emit-modrm-r/m state opcode-ext operand)))))
        (t
         (error "Operand must be a register, got ~S." operand))))

(defun emit-shift-by-immediate (state opcode-ext operands)
  (destructuring-bind (operand count) operands
    (let ((width (or (operand-width operand)
                     (memory-operand-width operand)
                     (error "Shift requires a register or explicitly-sized memory operand, got ~S."
                            operand))))
      (cond ((integerp count)
             (unless (typep count '(integer 0 255))
               (error "Shift count must fit in 8 bits, got ~S." count))
             (ecase width
               (8
                (emit-u8 state #xC0)
                (emit-modrm-r/m state opcode-ext operand)
                (emit-u8 state count))
               (16
                (emit-operand-size-prefix state 16)
                (emit-u8 state #xC1)
                (emit-modrm-r/m state opcode-ext operand)
                (emit-u8 state count))
               (32
                (emit-operand-size-prefix state 32)
                (emit-u8 state #xC1)
                (emit-modrm-r/m state opcode-ext operand)
                (emit-u8 state count))))
            ((and (typep count '(or symbol string))
                  (eql (token-keyword count) :CL))
             (ecase width
               (8
                (emit-u8 state #xD2)
                (emit-modrm-r/m state opcode-ext operand))
               (16
                (emit-operand-size-prefix state 16)
                (emit-u8 state #xD3)
                (emit-modrm-r/m state opcode-ext operand))
               (32
                (emit-operand-size-prefix state 32)
                (emit-u8 state #xD3)
                (emit-modrm-r/m state opcode-ext operand))))
            (t
             (error "Shift count must be an immediate integer or CL, got ~S." count))))))

(defun emit-conditional-jump (state opcode label)
  (emit-u8 state opcode)
  (emit-rel8 state label))

(defun emit-near-conditional-jump (state opcode label)
  (emit-u8 state #x0F)
  (emit-u8 state opcode)
  (if (= (assembly-bits state) 16)
      (emit-rel16 state label)
      (emit-rel32 state label)))

(defun emit-mov-immediate-to-r/m (state operand immediate width)
  (when (memory-operand-p operand)
    (ensure-memory-width operand width))
  (ecase width
    (8
     (emit-u8 state #xC6)
     (emit-modrm-r/m state 0 operand)
     (emit-imm8 state immediate))
    (16
     (emit-operand-size-prefix state 16)
     (emit-u8 state #xC7)
     (emit-modrm-r/m state 0 operand)
     (emit-imm16 state immediate))
    (32
     (emit-operand-size-prefix state 32)
     (emit-u8 state #xC7)
     (emit-modrm-r/m state 0 operand)
     (emit-imm32 state immediate))))

(defun emit-movx (state signedp destination source)
  (let ((destination-width (cond ((word-register-p destination) 16)
                                 ((dword-register-p destination) 32)
                                 (t
                                  (error "~A destination must be a 16-bit or 32-bit register, got ~S."
                                         (if signedp "MOVSX" "MOVZX")
                                         destination))))
        (source-width (cond ((byte-register-p source) 8)
                            ((word-register-p source) 16)
                            ((memory-operand-p source)
                             (or (memory-operand-width source)
                                 (error "~A from memory requires an explicit source width, got ~S."
                                        (if signedp "MOVSX" "MOVZX")
                                        source)))
                            (t
                             (error "~A source must be a byte/word register or memory operand, got ~S."
                                    (if signedp "MOVSX" "MOVZX")
                                    source)))))
    (when (and (= destination-width 16) (= source-width 16))
      (error "~A does not support 16-bit source to 16-bit destination." (if signedp "MOVSX" "MOVZX")))
    (emit-operand-size-prefix state destination-width)
    (emit-u8 state #x0F)
    (emit-u8 state (case source-width
                     (8 (if signedp #xBE #xB6))
                     (16 (if signedp #xBF #xB7))
                     (t (error "Unsupported source width ~D." source-width))))
    (emit-modrm-r/m state (register-code destination
                                         (if (= destination-width 16)
                                             +x86-word-registers+
                                             +x86-dword-registers+))
                   source)))

(defun encode-instruction (state operator operands)
  (flet ((expect-arity (count)
           (unless (= (length operands) count)
             (error "~S expects ~D operands, got ~D." operator count (length operands)))))
    (case (token-keyword operator)
      (:nop
       (expect-arity 0)
       (emit-u8 state #x90))
      (:cli
       (expect-arity 0)
       (emit-u8 state #xFA))
      (:sti
       (expect-arity 0)
       (emit-u8 state #xFB))
      (:cld
       (expect-arity 0)
       (emit-u8 state #xFC))
      (:hlt
       (expect-arity 0)
       (emit-u8 state #xF4))
      (:ret
       (expect-arity 0)
       (emit-u8 state #xC3))
      (:iret
       (expect-arity 0)
       (emit-u8 state #xCF))
      (:lodsb
       (expect-arity 0)
       (emit-u8 state #xAC))
      (:stosw
       (expect-arity 0)
       (emit-operand-size-prefix state 16)
       (emit-u8 state #xAB))
      (:int
       (expect-arity 1)
       (emit-u8 state #xCD)
       (emit-imm8 state (first operands)))
      (:out
       (expect-arity 2)
       (destructuring-bind (port source) operands
         (let ((width (cond ((eql (token-keyword source) :AL) 8)
                            ((eql (token-keyword source) :AX) 16)
                            ((eql (token-keyword source) :EAX) 32)
                            (t nil))))
           (cond ((and width
                       (integerp port)
                       (typep port '(integer 0 255)))
                  (emit-operand-size-prefix state width)
                  (emit-u8 state (if (= width 8) #xE6 #xE7))
                  (emit-u8 state port))
                 ((and width
                       (typep port '(or symbol string))
                       (eql (token-keyword port) :DX))
                  (emit-operand-size-prefix state width)
                 (emit-u8 state (if (= width 8) #xEE #xEF)))
                 (t
                  (error "OUT currently supports AL/AX/EAX with immediate-8 or DX ports, got ~S."
                         operands))))))
      (:in
       (expect-arity 2)
       (destructuring-bind (destination port) operands
         (let ((width (cond ((eql (token-keyword destination) :AL) 8)
                            ((eql (token-keyword destination) :AX) 16)
                            ((eql (token-keyword destination) :EAX) 32)
                            (t nil))))
           (cond ((and width
                       (integerp port)
                       (typep port '(integer 0 255)))
                  (emit-operand-size-prefix state width)
                  (emit-u8 state (if (= width 8) #xE4 #xE5))
                  (emit-u8 state port))
                 ((and width
                       (typep port '(or symbol string))
                       (eql (token-keyword port) :DX))
                  (emit-operand-size-prefix state width)
                  (emit-u8 state (if (= width 8) #xEC #xED)))
                 (t
                  (error "IN currently supports AL/AX/EAX with immediate-8 or DX ports, got ~S."
                         operands))))))
      (:call
       (expect-arity 1)
       (let ((operand (first operands)))
         (cond ((dword-register-p operand)
                (emit-u8 state #xFF)
                (emit-modrm state 3 2 (dword-register-p operand)))
               ((memory-operand-p operand)
                (emit-u8 state #xFF)
                (emit-modrm-r/m state 2 operand))
               (t
                (emit-u8 state #xE8)
                (if (= (assembly-bits state) 16)
                    (emit-rel16 state operand)
                    (emit-rel32 state operand))))))
      (:jmp
       (expect-arity 1)
       (let ((operand (first operands)))
         (cond ((dword-register-p operand)
                (emit-u8 state #xFF)
                (emit-modrm state 3 4 (dword-register-p operand)))
               ((memory-operand-p operand)
                (emit-u8 state #xFF)
                (emit-modrm-r/m state 4 operand))
               (t
                (emit-u8 state #xE9)
                (if (= (assembly-bits state) 16)
                    (emit-rel16 state operand)
                    (emit-rel32 state operand))))))
      (:jz
       (expect-arity 1)
       (emit-u8 state #x74)
       (emit-rel8 state (first operands)))
      (:jnz
       (expect-arity 1)
       (emit-u8 state #x75)
       (emit-rel8 state (first operands)))
      (:jc
       (expect-arity 1)
       (emit-u8 state #x72)
       (emit-rel8 state (first operands)))
      (:jnc
       (expect-arity 1)
       (emit-u8 state #x73)
       (emit-rel8 state (first operands)))
      (:ljmp
       (expect-arity 2)
       (emit-far-jump state (first operands) (second operands) (assembly-bits state)))
      (:ljmp16
       (expect-arity 2)
       (emit-far-jump state (first operands) (second operands) 16))
      (:ljmp32
       (expect-arity 2)
       (emit-far-jump state (first operands) (second operands) 32))
      (:lgdt
       (expect-arity 1)
       (emit-u8 state #x0F)
       (emit-u8 state #x01)
       (emit-absolute-memory-reference state 2 (first operands)))
      (:lidt
       (expect-arity 1)
       (emit-u8 state #x0F)
       (emit-u8 state #x01)
       (emit-absolute-memory-reference state 3 (first operands)))
      (:inc
       (expect-arity 1)
       (cond ((dword-register-p (first operands))
              (emit-unary-register-op state #x40 (first operands) 32 +x86-dword-registers+))
             ((word-register-p (first operands))
              (emit-unary-register-op state #x40 (first operands) 16 +x86-word-registers+))
             ((byte-register-p (first operands))
              (emit-u8 state #xFE)
              (emit-modrm state 3 0 (byte-register-p (first operands))))
             ((memory-operand-p (first operands))
              (let ((width (or (memory-operand-width (first operands))
                               (error "Memory INC requires an explicit width, got ~S."
                                      (first operands)))))
                (ecase width
                  (8
                   (emit-u8 state #xFE)
                   (emit-modrm-r/m state 0 (first operands)))
                  (16
                   (emit-operand-size-prefix state 16)
                   (emit-u8 state #xFF)
                   (emit-modrm-r/m state 0 (first operands)))
                  (32
                   (emit-operand-size-prefix state 32)
                   (emit-u8 state #xFF)
                   (emit-modrm-r/m state 0 (first operands))))))
             (t
              (error "INC requires a register or sized memory operand, got ~S."
                     (first operands)))))
      (:dec
       (expect-arity 1)
       (cond ((dword-register-p (first operands))
              (emit-unary-register-op state #x48 (first operands) 32 +x86-dword-registers+))
             ((word-register-p (first operands))
              (emit-unary-register-op state #x48 (first operands) 16 +x86-word-registers+))
             ((byte-register-p (first operands))
              (emit-u8 state #xFE)
              (emit-modrm state 3 1 (byte-register-p (first operands))))
             ((memory-operand-p (first operands))
              (let ((width (or (memory-operand-width (first operands))
                               (error "Memory DEC requires an explicit width, got ~S."
                                      (first operands)))))
                (ecase width
                  (8
                   (emit-u8 state #xFE)
                   (emit-modrm-r/m state 1 (first operands)))
                  (16
                   (emit-operand-size-prefix state 16)
                   (emit-u8 state #xFF)
                   (emit-modrm-r/m state 1 (first operands)))
                  (32
                   (emit-operand-size-prefix state 32)
                   (emit-u8 state #xFF)
                   (emit-modrm-r/m state 1 (first operands))))))
             (t
              (error "DEC requires a register or sized memory operand, got ~S."
                     (first operands)))))
      (:push
       (expect-arity 1)
       (cond ((dword-register-p (first operands))
              (emit-unary-register-op state #x50 (first operands) 32 +x86-dword-registers+))
             ((word-register-p (first operands))
              (emit-unary-register-op state #x50 (first operands) 16 +x86-word-registers+))
             (t
              (error "PUSH currently supports 16-bit and 32-bit registers, got ~S."
                     (first operands)))))
      (:pop
       (expect-arity 1)
       (cond ((dword-register-p (first operands))
              (emit-unary-register-op state #x58 (first operands) 32 +x86-dword-registers+))
             ((word-register-p (first operands))
              (emit-unary-register-op state #x58 (first operands) 16 +x86-word-registers+))
             (t
              (error "POP currently supports 16-bit and 32-bit registers, got ~S."
                     (first operands)))))
      (:mov
       (expect-arity 2)
       (destructuring-bind (destination source) operands
         (cond ((dword-register-p destination)
                (cond ((dword-register-p source)
                       (emit-register-register state #x89 destination source
                                              +x86-dword-registers+ +x86-dword-registers+ 32))
                      ((memory-operand-p source)
                       (emit-register-r/m state #x8B destination source 32 +x86-dword-registers+))
                      ((control-register-p source)
                       (emit-u8 state #x0F)
                       (emit-u8 state #x20)
                       (emit-modrm state 3
                                   (control-register-p source)
                                   (dword-register-p destination)))
                      (t
                       (emit-register-immediate state #xB8
                                                (dword-register-p destination)
                                                source
                                                32))))
               ((word-register-p destination)
                (cond ((word-register-p source)
                       (emit-register-register state #x89 destination source
                                              +x86-word-registers+ +x86-word-registers+ 16))
                      ((memory-operand-p source)
                       (emit-register-r/m state #x8B destination source 16 +x86-word-registers+))
                      ((segment-register-p source)
                       (emit-u8 state #x8C)
                       (emit-modrm state 3
                                   (segment-register-p source)
                                   (word-register-p destination)))
                      (t
                       (emit-register-immediate state #xB8
                                                (word-register-p destination)
                                                source
                                                16))))
               ((byte-register-p destination)
                (cond ((byte-register-p source)
                       (emit-register-register state #x88 destination source
                                              +x86-byte-registers+ +x86-byte-registers+ 8))
                      ((memory-operand-p source)
                       (emit-register-r/m state #x8A destination source 8 +x86-byte-registers+))
                      (t
                       (emit-register-immediate state #xB0
                                                (byte-register-p destination)
                                                source
                                                8))))
               ((memory-operand-p destination)
                (cond ((dword-register-p source)
                       (emit-r/m-register state #x89 destination source 32 +x86-dword-registers+))
                      ((word-register-p source)
                       (emit-r/m-register state #x89 destination source 16 +x86-word-registers+))
                      ((byte-register-p source)
                       (emit-r/m-register state #x88 destination source 8 +x86-byte-registers+))
                      (t
                       (let ((width (or (memory-operand-width destination)
                                        (error "MOV immediate to memory requires an explicit width, got ~S."
                                               destination))))
                         (emit-mov-immediate-to-r/m state destination source width)))))
               ((segment-register-p destination)
                (unless (word-register-p source)
                  (error "MOV into segment registers requires a 16-bit register source, got ~S." source))
                (emit-u8 state #x8E)
                (emit-modrm state 3
                            (segment-register-p destination)
                            (word-register-p source)))
               ((control-register-p destination)
                (unless (dword-register-p source)
                  (error "MOV into control registers requires a 32-bit register source, got ~S." source))
                (emit-u8 state #x0F)
                (emit-u8 state #x22)
                (emit-modrm state 3
                            (control-register-p destination)
                            (dword-register-p source)))
               (t
                (error "Unsupported MOV operands ~S." operands)))))
      (:lea
       (expect-arity 2)
       (destructuring-bind (destination source) operands
         (cond ((dword-register-p destination)
                (unless (memory-operand-p source)
                  (error "LEA requires a memory operand source, got ~S." source))
                (emit-register-r/m state #x8D destination source 32 +x86-dword-registers+))
               ((word-register-p destination)
                (unless (memory-operand-p source)
                  (error "LEA requires a memory operand source, got ~S." source))
                (emit-register-r/m state #x8D destination source 16 +x86-word-registers+))
               (t
                (error "LEA requires a 16-bit or 32-bit destination register, got ~S."
                       destination)))))
      (:movzx
       (expect-arity 2)
       (emit-movx state nil (first operands) (second operands)))
      (:movsx
       (expect-arity 2)
       (emit-movx state t (first operands) (second operands)))
      (:xor
       (expect-arity 2)
       (destructuring-bind (destination source) operands
         (cond ((and (dword-register-p destination) (dword-register-p source))
                (emit-register-register state #x31 destination source
                                       +x86-dword-registers+ +x86-dword-registers+ 32))
               ((and (dword-register-p destination) (memory-operand-p source))
                (emit-register-r/m state #x33 destination source 32 +x86-dword-registers+))
               ((and (memory-operand-p destination) (dword-register-p source))
                (emit-r/m-register state #x31 destination source 32 +x86-dword-registers+))
               ((and (word-register-p destination) (word-register-p source))
                (emit-register-register state #x31 destination source
                                       +x86-word-registers+ +x86-word-registers+ 16))
               ((and (word-register-p destination) (memory-operand-p source))
                (emit-register-r/m state #x33 destination source 16 +x86-word-registers+))
               ((and (memory-operand-p destination) (word-register-p source))
                (emit-r/m-register state #x31 destination source 16 +x86-word-registers+))
               ((and (byte-register-p destination) (byte-register-p source))
                (emit-register-register state #x30 destination source
                                       +x86-byte-registers+ +x86-byte-registers+ 8))
               ((and (byte-register-p destination) (memory-operand-p source))
                (emit-register-r/m state #x32 destination source 8 +x86-byte-registers+))
               ((and (memory-operand-p destination) (byte-register-p source))
                (emit-r/m-register state #x30 destination source 8 +x86-byte-registers+))
               ((or (dword-register-p destination)
                    (word-register-p destination)
                    (byte-register-p destination)
                    (memory-operand-p destination))
                (emit-group1-immediate state 6 destination source
                                       (cond ((dword-register-p destination) 32)
                                             ((word-register-p destination) 16)
                                             ((byte-register-p destination) 8)
                                             (t (or (memory-operand-width destination)
                                                    (error "Immediate XOR to memory requires an explicit width, got ~S."
                                                           destination))))))
               (t
                (error "Unsupported XOR operands ~S." operands)))))
      (:or
       (expect-arity 2)
       (destructuring-bind (destination source) operands
         (cond ((and (dword-register-p destination) (dword-register-p source))
                (emit-register-register state #x09 destination source
                                       +x86-dword-registers+ +x86-dword-registers+ 32))
               ((and (dword-register-p destination) (memory-operand-p source))
                (emit-register-r/m state #x0B destination source 32 +x86-dword-registers+))
               ((and (memory-operand-p destination) (dword-register-p source))
                (emit-r/m-register state #x09 destination source 32 +x86-dword-registers+))
               ((and (word-register-p destination) (word-register-p source))
                (emit-register-register state #x09 destination source
                                       +x86-word-registers+ +x86-word-registers+ 16))
               ((and (word-register-p destination) (memory-operand-p source))
                (emit-register-r/m state #x0B destination source 16 +x86-word-registers+))
               ((and (memory-operand-p destination) (word-register-p source))
                (emit-r/m-register state #x09 destination source 16 +x86-word-registers+))
               ((and (byte-register-p destination) (byte-register-p source))
                (emit-register-register state #x08 destination source
                                       +x86-byte-registers+ +x86-byte-registers+ 8))
               ((and (byte-register-p destination) (memory-operand-p source))
                (emit-register-r/m state #x0A destination source 8 +x86-byte-registers+))
               ((and (memory-operand-p destination) (byte-register-p source))
                (emit-r/m-register state #x08 destination source 8 +x86-byte-registers+))
               ((or (dword-register-p destination)
                    (word-register-p destination)
                    (byte-register-p destination)
                    (memory-operand-p destination))
                (emit-group1-immediate state 1 destination source
                                       (cond ((dword-register-p destination) 32)
                                             ((word-register-p destination) 16)
                                             ((byte-register-p destination) 8)
                                             (t (or (memory-operand-width destination)
                                                    (error "Immediate OR to memory requires an explicit width, got ~S."
                                                           destination))))))
               (t
                (error "Unsupported OR operands ~S." operands)))))
      (:and
       (expect-arity 2)
       (destructuring-bind (destination source) operands
         (cond ((and (dword-register-p destination) (dword-register-p source))
                (emit-register-register state #x21 destination source
                                       +x86-dword-registers+ +x86-dword-registers+ 32))
               ((and (dword-register-p destination) (memory-operand-p source))
                (emit-register-r/m state #x23 destination source 32 +x86-dword-registers+))
               ((and (memory-operand-p destination) (dword-register-p source))
                (emit-r/m-register state #x21 destination source 32 +x86-dword-registers+))
               ((and (word-register-p destination) (word-register-p source))
                (emit-register-register state #x21 destination source
                                       +x86-word-registers+ +x86-word-registers+ 16))
               ((and (word-register-p destination) (memory-operand-p source))
                (emit-register-r/m state #x23 destination source 16 +x86-word-registers+))
               ((and (memory-operand-p destination) (word-register-p source))
                (emit-r/m-register state #x21 destination source 16 +x86-word-registers+))
               ((and (byte-register-p destination) (byte-register-p source))
                (emit-register-register state #x20 destination source
                                       +x86-byte-registers+ +x86-byte-registers+ 8))
               ((and (byte-register-p destination) (memory-operand-p source))
                (emit-register-r/m state #x22 destination source 8 +x86-byte-registers+))
               ((and (memory-operand-p destination) (byte-register-p source))
                (emit-r/m-register state #x20 destination source 8 +x86-byte-registers+))
               ((or (dword-register-p destination)
                    (word-register-p destination)
                    (byte-register-p destination)
                    (memory-operand-p destination))
                (emit-group1-immediate state 4 destination source
                                       (cond ((dword-register-p destination) 32)
                                             ((word-register-p destination) 16)
                                             ((byte-register-p destination) 8)
                                             (t (or (memory-operand-width destination)
                                                    (error "Immediate AND to memory requires an explicit width, got ~S."
                                                           destination))))))
               (t
                (error "Unsupported AND operands ~S." operands)))))
      (:cmp
       (expect-arity 2)
       (destructuring-bind (destination source) operands
         (cond ((and (dword-register-p destination) (dword-register-p source))
                (emit-register-register state #x39 destination source
                                       +x86-dword-registers+ +x86-dword-registers+ 32))
               ((and (dword-register-p destination) (memory-operand-p source))
                (emit-register-r/m state #x3B destination source 32 +x86-dword-registers+))
               ((and (memory-operand-p destination) (dword-register-p source))
                (emit-r/m-register state #x39 destination source 32 +x86-dword-registers+))
               ((and (word-register-p destination) (word-register-p source))
                (emit-register-register state #x39 destination source
                                       +x86-word-registers+ +x86-word-registers+ 16))
               ((and (word-register-p destination) (memory-operand-p source))
                (emit-register-r/m state #x3B destination source 16 +x86-word-registers+))
               ((and (memory-operand-p destination) (word-register-p source))
                (emit-r/m-register state #x39 destination source 16 +x86-word-registers+))
               ((and (byte-register-p destination) (byte-register-p source))
                (emit-register-register state #x38 destination source
                                       +x86-byte-registers+ +x86-byte-registers+ 8))
               ((and (byte-register-p destination) (memory-operand-p source))
                (emit-register-r/m state #x3A destination source 8 +x86-byte-registers+))
               ((and (memory-operand-p destination) (byte-register-p source))
                (emit-r/m-register state #x38 destination source 8 +x86-byte-registers+))
               ((dword-register-p destination)
                (emit-group1-immediate state 7 destination source 32))
               ((word-register-p destination)
                (emit-group1-immediate state 7 destination source 16))
               ((byte-register-p destination)
                (emit-group1-immediate state 7 destination source 8))
               ((memory-operand-p destination)
                (emit-group1-immediate state 7 destination source
                                       (or (memory-operand-width destination)
                                           (error "Immediate CMP to memory requires an explicit width, got ~S."
                                                  destination))))
               (t
                (error "Unsupported CMP operands ~S." operands)))))
      (:add
       (expect-arity 2)
       (destructuring-bind (destination source) operands
         (cond ((and (dword-register-p destination) (dword-register-p source))
                (emit-register-register state #x01 destination source
                                       +x86-dword-registers+ +x86-dword-registers+ 32))
               ((and (dword-register-p destination) (memory-operand-p source))
                (emit-register-r/m state #x03 destination source 32 +x86-dword-registers+))
               ((and (memory-operand-p destination) (dword-register-p source))
                (emit-r/m-register state #x01 destination source 32 +x86-dword-registers+))
               ((and (word-register-p destination) (word-register-p source))
                (emit-register-register state #x01 destination source
                                       +x86-word-registers+ +x86-word-registers+ 16))
               ((and (word-register-p destination) (memory-operand-p source))
                (emit-register-r/m state #x03 destination source 16 +x86-word-registers+))
               ((and (memory-operand-p destination) (word-register-p source))
                (emit-r/m-register state #x01 destination source 16 +x86-word-registers+))
               ((and (byte-register-p destination) (byte-register-p source))
                (emit-register-register state #x00 destination source
                                       +x86-byte-registers+ +x86-byte-registers+ 8))
               ((and (byte-register-p destination) (memory-operand-p source))
                (emit-register-r/m state #x02 destination source 8 +x86-byte-registers+))
               ((and (memory-operand-p destination) (byte-register-p source))
                (emit-r/m-register state #x00 destination source 8 +x86-byte-registers+))
               ((dword-register-p destination)
                (emit-group1-immediate state 0 destination source 32))
               ((word-register-p destination)
                (emit-group1-immediate state 0 destination source 16))
               ((byte-register-p destination)
                (emit-group1-immediate state 0 destination source 8))
               ((memory-operand-p destination)
                (emit-group1-immediate state 0 destination source
                                       (or (memory-operand-width destination)
                                           (error "Immediate ADD to memory requires an explicit width, got ~S."
                                                  destination))))
               (t
                (error "Unsupported ADD operands ~S." operands)))))
      (:sub
       (expect-arity 2)
       (destructuring-bind (destination source) operands
         (cond ((and (dword-register-p destination) (dword-register-p source))
                (emit-register-register state #x29 destination source
                                       +x86-dword-registers+ +x86-dword-registers+ 32))
               ((and (dword-register-p destination) (memory-operand-p source))
                (emit-register-r/m state #x2B destination source 32 +x86-dword-registers+))
               ((and (memory-operand-p destination) (dword-register-p source))
                (emit-r/m-register state #x29 destination source 32 +x86-dword-registers+))
               ((and (word-register-p destination) (word-register-p source))
                (emit-register-register state #x29 destination source
                                       +x86-word-registers+ +x86-word-registers+ 16))
               ((and (word-register-p destination) (memory-operand-p source))
                (emit-register-r/m state #x2B destination source 16 +x86-word-registers+))
               ((and (memory-operand-p destination) (word-register-p source))
                (emit-r/m-register state #x29 destination source 16 +x86-word-registers+))
               ((and (byte-register-p destination) (byte-register-p source))
                (emit-register-register state #x28 destination source
                                       +x86-byte-registers+ +x86-byte-registers+ 8))
               ((and (byte-register-p destination) (memory-operand-p source))
                (emit-register-r/m state #x2A destination source 8 +x86-byte-registers+))
               ((and (memory-operand-p destination) (byte-register-p source))
                (emit-r/m-register state #x28 destination source 8 +x86-byte-registers+))
               ((dword-register-p destination)
                (emit-group1-immediate state 5 destination source 32))
               ((word-register-p destination)
                (emit-group1-immediate state 5 destination source 16))
               ((byte-register-p destination)
                (emit-group1-immediate state 5 destination source 8))
               ((memory-operand-p destination)
                (emit-group1-immediate state 5 destination source
                                       (or (memory-operand-width destination)
                                           (error "Immediate SUB to memory requires an explicit width, got ~S."
                                                  destination))))
               (t
                (error "Unsupported SUB operands ~S." operands)))))
      (:test
       (expect-arity 2)
       (destructuring-bind (destination source) operands
         (cond ((and (dword-register-p destination) (dword-register-p source))
                (emit-register-register state #x85 destination source
                                       +x86-dword-registers+ +x86-dword-registers+ 32))
               ((and (dword-register-p destination) (memory-operand-p source))
                (emit-register-r/m state #x85 destination source 32 +x86-dword-registers+))
               ((and (memory-operand-p destination) (dword-register-p source))
                (emit-r/m-register state #x85 destination source 32 +x86-dword-registers+))
               ((and (word-register-p destination) (word-register-p source))
                (emit-register-register state #x85 destination source
                                       +x86-word-registers+ +x86-word-registers+ 16))
               ((and (word-register-p destination) (memory-operand-p source))
                (emit-register-r/m state #x85 destination source 16 +x86-word-registers+))
               ((and (memory-operand-p destination) (word-register-p source))
                (emit-r/m-register state #x85 destination source 16 +x86-word-registers+))
               ((and (byte-register-p destination) (byte-register-p source))
                (emit-register-register state #x84 destination source
                                       +x86-byte-registers+ +x86-byte-registers+ 8))
               ((and (byte-register-p destination) (memory-operand-p source))
                (emit-register-r/m state #x84 destination source 8 +x86-byte-registers+))
               ((and (memory-operand-p destination) (byte-register-p source))
                (emit-r/m-register state #x84 destination source 8 +x86-byte-registers+))
               ((dword-register-p destination)
                (emit-operand-size-prefix state 32)
                (emit-u8 state #xF7)
                (emit-modrm-r/m state 0 destination)
                (emit-imm32 state source))
               ((word-register-p destination)
                (emit-operand-size-prefix state 16)
                (emit-u8 state #xF7)
                (emit-modrm-r/m state 0 destination)
                (emit-imm16 state source))
               ((byte-register-p destination)
                (emit-u8 state #xF6)
                (emit-modrm-r/m state 0 destination)
                (emit-imm8 state source))
               ((memory-operand-p destination)
                (let ((width (or (memory-operand-width destination)
                                 (error "Immediate TEST to memory requires an explicit width, got ~S."
                                        destination))))
                  (ecase width
                    (8
                     (emit-u8 state #xF6)
                     (emit-modrm-r/m state 0 destination)
                     (emit-imm8 state source))
                    (16
                     (emit-operand-size-prefix state 16)
                     (emit-u8 state #xF7)
                     (emit-modrm-r/m state 0 destination)
                     (emit-imm16 state source))
                    (32
                     (emit-operand-size-prefix state 32)
                     (emit-u8 state #xF7)
                     (emit-modrm-r/m state 0 destination)
                     (emit-imm32 state source)))))
               (t
                (error "Unsupported TEST operands ~S." operands)))))
      (:shl
       (expect-arity 2)
       (emit-shift-by-immediate state 4 operands))
      (:shr
       (expect-arity 2)
       (emit-shift-by-immediate state 5 operands))
      (:stosb
       (expect-arity 0)
       (emit-u8 state #xAA))
      (:stosd
       (expect-arity 0)
       (emit-operand-size-prefix state 32)
       (emit-u8 state #xAB))
      (:movsd
       (expect-arity 0)
       (emit-operand-size-prefix state 32)
       (emit-u8 state #xA5))
      (:insb
       (expect-arity 0)
       (emit-u8 state #x6C))
      (:insw
       (expect-arity 0)
       (emit-operand-size-prefix state 16)
       (emit-u8 state #x6D))
      (:insd
       (expect-arity 0)
       (emit-operand-size-prefix state 32)
       (emit-u8 state #x6D))
      (:outsb
       (expect-arity 0)
       (emit-u8 state #x6E))
      (:outsw
       (expect-arity 0)
       (emit-operand-size-prefix state 16)
       (emit-u8 state #x6F))
      (:outsd
       (expect-arity 0)
       (emit-operand-size-prefix state 32)
       (emit-u8 state #x6F))
      (:rep
       (expect-arity 1)
       (emit-u8 state #xF3)
       (encode-instruction state (first operands) '()))
      (:lock
       (when (null operands)
         (error "LOCK requires an instruction to wrap."))
       (emit-u8 state #xF0)
       (encode-instruction state (first operands) (rest operands)))
      (:mfence
       (expect-arity 0)
       (emit-u8 state #x0F)
       (emit-u8 state #xAE)
       (emit-u8 state #xF0))
      (:lfence
       (expect-arity 0)
       (emit-u8 state #x0F)
       (emit-u8 state #xAE)
       (emit-u8 state #xE8))
      (:sfence
       (expect-arity 0)
       (emit-u8 state #x0F)
       (emit-u8 state #xAE)
       (emit-u8 state #xF8))
      (:wrmsr
       (expect-arity 0)
       (emit-u8 state #x0F)
       (emit-u8 state #x30))
      (:rdmsr
       (expect-arity 0)
       (emit-u8 state #x0F)
       (emit-u8 state #x32))
      (:clts
       (expect-arity 0)
       (emit-u8 state #x0F)
       (emit-u8 state #x06))
      (:invlpg
       (expect-arity 1)
       (let ((operand (first operands)))
         (unless (memory-operand-p operand)
           (error "INVLPG requires a memory operand, got ~S." operand))
         (emit-u8 state #x0F)
         (emit-u8 state #x01)
         (emit-modrm-r/m state 7 operand)))
      (:ltr
       (expect-arity 1)
       (let ((operand (first operands)))
         (emit-u8 state #x0F)
         (emit-u8 state #x00)
         (cond ((word-register-p operand)
                (emit-modrm state 3 3 (word-register-p operand)))
               ((memory-operand-p operand)
                (ensure-memory-width operand 16)
                (emit-modrm-r/m state 3 operand))
               (t
                (error "LTR requires a 16-bit register or memory operand, got ~S." operand)))))
      (:retf
       (cond ((null operands)
              (emit-u8 state #xCB))
             ((= (length operands) 1)
              (emit-u8 state #xCA)
              (emit-imm16 state (first operands)))
             (t
              (error "RETF expects 0 or 1 operand, got ~D." (length operands)))))
      (:lret
       (cond ((null operands)
              (emit-u8 state #xCB))
             ((= (length operands) 1)
              (emit-u8 state #xCA)
              (emit-imm16 state (first operands)))
             (t
              (error "LRET expects 0 or 1 operand, got ~D." (length operands)))))
      (:cmpxchg
       (expect-arity 2)
       (destructuring-bind (destination source) operands
         (cond ((and (or (dword-register-p destination) (memory-operand-p destination))
                     (dword-register-p source))
                (when (memory-operand-p destination)
                  (ensure-memory-width destination 32))
                (emit-operand-size-prefix state 32)
                (emit-u8 state #x0F)
                (emit-u8 state #xB1)
                (emit-modrm-r/m state (dword-register-p source) destination))
               ((and (or (word-register-p destination) (memory-operand-p destination))
                     (word-register-p source))
                (when (memory-operand-p destination)
                  (ensure-memory-width destination 16))
                (emit-operand-size-prefix state 16)
                (emit-u8 state #x0F)
                (emit-u8 state #xB1)
                (emit-modrm-r/m state (word-register-p source) destination))
               ((and (or (byte-register-p destination) (memory-operand-p destination))
                     (byte-register-p source))
                (when (memory-operand-p destination)
                  (ensure-memory-width destination 8))
                (emit-u8 state #x0F)
                (emit-u8 state #xB0)
                (emit-modrm-r/m state (byte-register-p source) destination))
               (t
                (error "Unsupported CMPXCHG operands ~S." operands)))))
      (:cpuid
       (expect-arity 0)
       (emit-u8 state #x0F)
       (emit-u8 state #xA2))
      (:rdtsc
       (expect-arity 0)
       (emit-u8 state #x0F)
       (emit-u8 state #x31))
      (:pusha
       (expect-arity 0)
       (emit-u8 state #x60))
      (:pushad
       (expect-arity 0)
       (emit-u8 state #x60))
      (:popa
       (expect-arity 0)
       (emit-u8 state #x61))
      (:popad
       (expect-arity 0)
       (emit-u8 state #x61))
      (:pushfd
       (expect-arity 0)
       (emit-u8 state #x9C))
      (:popfd
       (expect-arity 0)
       (emit-u8 state #x9D))
      (:mul
       (expect-arity 1)
       (emit-unary-rm-op state 4 (first operands)))
      (:div
       (expect-arity 1)
       (emit-unary-rm-op state 6 (first operands)))
      (:not
       (expect-arity 1)
       (emit-unary-rm-op state 2 (first operands)))
      (:neg
       (expect-arity 1)
       (emit-unary-rm-op state 3 (first operands)))
      (:xchg
       (expect-arity 2)
       (destructuring-bind (left right) operands
         (let ((width (or (operand-width left)
                          (operand-width right)
                          (memory-operand-width left)
                          (memory-operand-width right)
                          (error "XCHG requires at least one register or explicitly-sized memory operand, got ~S."
                                 operands))))
           (ecase width
             (8
              (cond ((byte-register-p right)
                     (emit-r/m-register state #x86 left right 8 +x86-byte-registers+))
                    ((byte-register-p left)
                     (emit-r/m-register state #x86 right left 8 +x86-byte-registers+))
                    (t
                     (error "XCHG byte form requires a byte register operand, got ~S." operands))))
             (16
              (cond ((word-register-p right)
                     (emit-r/m-register state #x87 left right 16 +x86-word-registers+))
                    ((word-register-p left)
                     (emit-r/m-register state #x87 right left 16 +x86-word-registers+))
                    (t
                     (error "XCHG word form requires a 16-bit register operand, got ~S." operands))))
             (32
              (cond ((dword-register-p right)
                     (emit-r/m-register state #x87 left right 32 +x86-dword-registers+))
                    ((dword-register-p left)
                     (emit-r/m-register state #x87 right left 32 +x86-dword-registers+))
                    (t
                     (error "XCHG dword form requires a 32-bit register operand, got ~S."
                            operands))))))))
      (:loop
       (expect-arity 1)
       (emit-u8 state #xE2)
       (emit-rel8 state (first operands)))
      (:je
       (expect-arity 1)
       (emit-conditional-jump state #x74 (first operands)))
      (:jne
       (expect-arity 1)
       (emit-conditional-jump state #x75 (first operands)))
      (:jb
       (expect-arity 1)
       (emit-conditional-jump state #x72 (first operands)))
      (:jnb
       (expect-arity 1)
       (emit-conditional-jump state #x73 (first operands)))
      (:jae
       (expect-arity 1)
       (emit-conditional-jump state #x73 (first operands)))
      (:jbe
       (expect-arity 1)
       (emit-conditional-jump state #x76 (first operands)))
      (:ja
       (expect-arity 1)
       (emit-conditional-jump state #x77 (first operands)))
      (:js
       (expect-arity 1)
       (emit-conditional-jump state #x78 (first operands)))
      (:jns
       (expect-arity 1)
       (emit-conditional-jump state #x79 (first operands)))
      (:jl
       (expect-arity 1)
       (emit-conditional-jump state #x7C (first operands)))
      (:jge
       (expect-arity 1)
       (emit-conditional-jump state #x7D (first operands)))
      (:jle
       (expect-arity 1)
       (emit-conditional-jump state #x7E (first operands)))
      (:jg
       (expect-arity 1)
       (emit-conditional-jump state #x7F (first operands)))
      (:jz-near
       (expect-arity 1)
       (emit-near-conditional-jump state #x84 (first operands)))
      (:jnz-near
       (expect-arity 1)
       (emit-near-conditional-jump state #x85 (first operands)))
      (:je-near
       (expect-arity 1)
       (emit-near-conditional-jump state #x84 (first operands)))
      (:jne-near
       (expect-arity 1)
       (emit-near-conditional-jump state #x85 (first operands)))
      (:jc-near
       (expect-arity 1)
       (emit-near-conditional-jump state #x82 (first operands)))
      (:jnc-near
       (expect-arity 1)
       (emit-near-conditional-jump state #x83 (first operands)))
      (:jb-near
       (expect-arity 1)
       (emit-near-conditional-jump state #x82 (first operands)))
      (:jnb-near
       (expect-arity 1)
       (emit-near-conditional-jump state #x83 (first operands)))
      (:jae-near
       (expect-arity 1)
       (emit-near-conditional-jump state #x83 (first operands)))
      (:jbe-near
       (expect-arity 1)
       (emit-near-conditional-jump state #x86 (first operands)))
      (:ja-near
       (expect-arity 1)
       (emit-near-conditional-jump state #x87 (first operands)))
      (:js-near
       (expect-arity 1)
       (emit-near-conditional-jump state #x88 (first operands)))
      (:jns-near
       (expect-arity 1)
       (emit-near-conditional-jump state #x89 (first operands)))
      (:jl-near
       (expect-arity 1)
       (emit-near-conditional-jump state #x8C (first operands)))
      (:jge-near
       (expect-arity 1)
       (emit-near-conditional-jump state #x8D (first operands)))
      (:jle-near
       (expect-arity 1)
       (emit-near-conditional-jump state #x8E (first operands)))
      (:jg-near
       (expect-arity 1)
       (emit-near-conditional-jump state #x8F (first operands)))
      (t
       (error "Unknown instruction ~S." operator)))))
