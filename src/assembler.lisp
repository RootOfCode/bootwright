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

(defun emit-modrm (state mod reg rm)
  (emit-u8 state (logior (ash mod 6)
                         (ash reg 3)
                         rm)))

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

(defun emit-group1-immediate (state opcode-ext register operand width)
  (let ((reg (or (dword-register-p register)
                 (word-register-p register)
                 (byte-register-p register))))
    (unless reg
      (error "Unsupported operand ~S for grouped arithmetic." register))
    (ecase width
      (8
       (emit-u8 state #x80)
       (emit-modrm state 3 opcode-ext reg)
       (emit-imm8 state operand))
      (16
       (emit-operand-size-prefix state 16)
       (emit-u8 state #x81)
       (emit-modrm state 3 opcode-ext reg)
       (emit-imm16 state operand))
      (32
       (emit-operand-size-prefix state 32)
       (emit-u8 state #x81)
       (emit-modrm state 3 opcode-ext reg)
       (emit-imm32 state operand)))))

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
         (cond ((and (integerp port)
                     (typep port '(integer 0 255))
                     (eql (token-keyword source) :AL))
                (emit-u8 state #xE6)
                (emit-u8 state port))
               ((and (typep port '(or symbol string))
                     (eql (token-keyword port) :DX)
                     (eql (token-keyword source) :AL))
                (emit-u8 state #xEE))
               (t
                (error "OUT currently supports immediate-8 or DX ports with AL, got ~S."
                       operands)))))
      (:in
       (expect-arity 2)
       (destructuring-bind (destination port) operands
         (cond ((and (eql (token-keyword destination) :AL)
                     (integerp port)
                     (typep port '(integer 0 255)))
                (emit-u8 state #xE4)
                (emit-u8 state port))
               ((and (eql (token-keyword destination) :AL)
                     (typep port '(or symbol string))
                     (eql (token-keyword port) :DX))
                (emit-u8 state #xEC))
               (t
                (error "IN currently supports AL with immediate-8 or DX ports, got ~S."
                       operands)))))
      (:call
       (expect-arity 1)
       (emit-u8 state #xE8)
       (if (= (assembly-bits state) 16)
           (emit-rel16 state (first operands))
           (emit-rel32 state (first operands))))
      (:jmp
       (expect-arity 1)
       (emit-u8 state #xE9)
       (if (= (assembly-bits state) 16)
           (emit-rel16 state (first operands))
           (emit-rel32 state (first operands))))
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
             (t
              (error "INC currently supports 16-bit and 32-bit registers, got ~S."
                     (first operands)))))
      (:dec
       (expect-arity 1)
       (cond ((dword-register-p (first operands))
              (emit-unary-register-op state #x48 (first operands) 32 +x86-dword-registers+))
             ((word-register-p (first operands))
              (emit-unary-register-op state #x48 (first operands) 16 +x86-word-registers+))
             (t
              (error "DEC currently supports 16-bit and 32-bit registers, got ~S."
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
                (if (byte-register-p source)
                    (emit-register-register state #x88 destination source
                                           +x86-byte-registers+ +x86-byte-registers+ 8)
                    (emit-register-immediate state #xB0
                                             (byte-register-p destination)
                                             source
                                             8)))
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
      (:xor
       (expect-arity 2)
       (destructuring-bind (destination source) operands
         (cond ((and (dword-register-p destination) (dword-register-p source))
                (emit-register-register state #x31 destination source
                                       +x86-dword-registers+ +x86-dword-registers+ 32))
               ((and (word-register-p destination) (word-register-p source))
                (emit-register-register state #x31 destination source
                                       +x86-word-registers+ +x86-word-registers+ 16))
               ((and (byte-register-p destination) (byte-register-p source))
                (emit-register-register state #x30 destination source
                                       +x86-byte-registers+ +x86-byte-registers+ 8))
               ((or (dword-register-p destination)
                    (word-register-p destination)
                    (byte-register-p destination))
                (emit-group1-immediate state 6 destination source
                                       (cond ((dword-register-p destination) 32)
                                             ((word-register-p destination) 16)
                                             (t 8))))
               (t
                (error "Unsupported XOR operands ~S." operands)))))
      (:or
       (expect-arity 2)
       (destructuring-bind (destination source) operands
         (cond ((and (dword-register-p destination) (dword-register-p source))
                (emit-register-register state #x09 destination source
                                       +x86-dword-registers+ +x86-dword-registers+ 32))
               ((and (word-register-p destination) (word-register-p source))
                (emit-register-register state #x09 destination source
                                       +x86-word-registers+ +x86-word-registers+ 16))
               ((and (byte-register-p destination) (byte-register-p source))
                (emit-register-register state #x08 destination source
                                       +x86-byte-registers+ +x86-byte-registers+ 8))
               ((or (dword-register-p destination)
                    (word-register-p destination)
                    (byte-register-p destination))
                (emit-group1-immediate state 1 destination source
                                       (cond ((dword-register-p destination) 32)
                                             ((word-register-p destination) 16)
                                             (t 8))))
               (t
                (error "Unsupported OR operands ~S." operands)))))
      (:and
       (expect-arity 2)
       (destructuring-bind (destination source) operands
         (cond ((and (dword-register-p destination) (dword-register-p source))
                (emit-register-register state #x21 destination source
                                       +x86-dword-registers+ +x86-dword-registers+ 32))
               ((and (word-register-p destination) (word-register-p source))
                (emit-register-register state #x21 destination source
                                       +x86-word-registers+ +x86-word-registers+ 16))
               ((and (byte-register-p destination) (byte-register-p source))
                (emit-register-register state #x20 destination source
                                       +x86-byte-registers+ +x86-byte-registers+ 8))
               ((or (dword-register-p destination)
                    (word-register-p destination)
                    (byte-register-p destination))
                (emit-group1-immediate state 4 destination source
                                       (cond ((dword-register-p destination) 32)
                                             ((word-register-p destination) 16)
                                             (t 8))))
               (t
                (error "Unsupported AND operands ~S." operands)))))
      (:cmp
       (expect-arity 2)
       (destructuring-bind (destination source) operands
         (cond ((and (dword-register-p destination) (dword-register-p source))
                (emit-register-register state #x39 destination source
                                       +x86-dword-registers+ +x86-dword-registers+ 32))
               ((and (word-register-p destination) (word-register-p source))
                (emit-register-register state #x39 destination source
                                       +x86-word-registers+ +x86-word-registers+ 16))
               ((and (byte-register-p destination) (byte-register-p source))
                (emit-register-register state #x38 destination source
                                       +x86-byte-registers+ +x86-byte-registers+ 8))
               ((dword-register-p destination)
                (emit-group1-immediate state 7 destination source 32))
               ((word-register-p destination)
                (emit-group1-immediate state 7 destination source 16))
               ((byte-register-p destination)
                (emit-group1-immediate state 7 destination source 8))
               (t
                (error "Unsupported CMP operands ~S." operands)))))
      (t
       (error "Unknown instruction ~S." operator)))))
