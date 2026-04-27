(require "asdf")

(let* ((root (make-pathname :name nil :type nil :defaults (or *load-truename*
                                                              *compile-file-truename*)))
       (asd (merge-pathnames #P"bootwright.asd" root)))
  (asdf:load-asd asd)
  (asdf:operate 'asdf:load-source-op :bootwright)
  ;; Phase 1 byte-level encoder tests — exercise the ISA dispatch through
  ;; assembly-state directly (no QEMU).  Each case asserts the exact octets
  ;; emitted for an architecture/instruction pair.
  (labels ((make-state (isa)
             (funcall (symbol-function (find-symbol "MAKE-ASSEMBLY-STATE" "BOOTWRIGHT"))
                      :isa isa))
           (encode (state op operands)
             (funcall (symbol-function (find-symbol "ENCODE-INSTRUCTION" "BOOTWRIGHT"))
                      state op operands))
           (state-bytes (state)
             (funcall (symbol-function (find-symbol "ASSEMBLY-STATE-BYTES" "BOOTWRIGHT"))
                      state))
           (assert-bytes (state expected label)
             (let ((actual (coerce (state-bytes state) 'list)))
               (unless (equal actual expected)
                 (error "Phase 1 ~A failed: expected ~S got ~S." label expected actual)))))
    ;; x86-64
    (let ((s (make-state :x86-64)))
      (encode s 'mov '(rax rbx))
      (assert-bytes s '(#x48 #x89 #xD8) "x86-64 MOV rax, rbx"))
    (let ((s (make-state :x86-64)))
      (encode s 'mov '(r8 r9))
      (assert-bytes s '(#x4D #x89 #xC8) "x86-64 MOV r8, r9"))
    (let ((s (make-state :x86-64)))
      (encode s 'push '(rax))
      (encode s 'pop '(r15))
      (assert-bytes s '(#x50 #x41 #x5F) "x86-64 PUSH rax; POP r15"))
    (let ((s (make-state :x86-64)))
      (encode s 'mov '(rax #x1122334455667788))
      (assert-bytes s '(#x48 #xB8 #x88 #x77 #x66 #x55 #x44 #x33 #x22 #x11)
                    "x86-64 movabs rax, 0x1122334455667788"))
    (let ((s (make-state :x86-64)))
      (encode s 'mov '(rax (:mem :rip #x10)))
      (assert-bytes s '(#x48 #x8B #x05 #x10 #x00 #x00 #x00) "x86-64 MOV rax, [rip+0x10]"))
    (let ((s (make-state :x86-64)))
      (encode s 'syscall '())
      (encode s 'sysret '())
      (encode s 'ret '())
      (assert-bytes s '(#x0F #x05 #x0F #x07 #xC3) "x86-64 syscall; sysret; ret"))
    ;; AArch64 stub
    (let ((s (make-state :aarch64)))
      (encode s 'nop '())
      (assert-bytes s '(#x1F #x20 #x03 #xD5) "aarch64 NOP"))
    (let ((s (make-state :aarch64)))
      (encode s 'ret '())
      (assert-bytes s '(#xC0 #x03 #x5F #xD6) "aarch64 RET"))
    (let ((s (make-state :aarch64)))
      (encode s 'mov '(x0 #x2A))
      (assert-bytes s '(#x40 #x05 #x80 #xD2) "aarch64 MOV x0, #42"))
    (let ((s (make-state :aarch64)))
      (encode s 'br '(x30))
      (assert-bytes s '(#xC0 #x03 #x1F #xD6) "aarch64 BR x30"))
    (let ((s (make-state :aarch64)))
      (encode s 'svc '(0))
      (assert-bytes s '(#x01 #x00 #x00 #xD4) "aarch64 SVC #0"))
    (format t "Bootwright Phase 1 byte-level tests passed.~%"))
  (labels ((test-source (source &rest args)
             (apply (symbol-function (find-symbol "TEST-OS-SOURCE-FILE" "BOOTWRIGHT"))
                    (merge-pathnames source root)
                    args)))
    (test-source "examples/basics/demo-real.bwo"
                 :timeout-ms 3000
                 :expect-debugcon '("stage1: loading real-mode demo"
                                    "kernel: real-mode demo reached"))
    (test-source "examples/protected/demo-protected.bwo"
                 :timeout-ms 5000
                 :expect-debugcon '("stage1: loading protected-mode demo"
                                    "kernel: protected mode + paging active"
                                    "kernel: returned from int 0x30"))
    (test-source "examples/basics/demo-assets.bwo"
                 :timeout-ms 3000
                 :expect-debugcon '("stage1: loading asset demo"
                                    "kernel: asset demo reached"))
    (test-source "examples/devices/demo-timer.bwo"
                 :timeout-ms 5000
                 :expect-debugcon '("stage1: loading timer demo"
                                    "kernel: PIT programmed and IRQ0 unmasked"
                                    "kernel: timer advanced one second"))
    (test-source "examples/devices/demo-probe.bwo"
                 :timeout-ms 5000
                 :expect-serial '("[probe] COM1 online @ 38400 8N1"
                                  "[probe] PIT 20 Hz armed; idling."
                                  "[probe] +1 s"))
    (test-source "examples/cpu/demo-cpuinfo.bwo"
                 :timeout-ms 3000
                 :expect-debugcon '("stage1: loading cpuinfo demo"
                                    "kernel: cpuinfo dump complete"))
    (test-source "examples/cpu/demo-bench.bwo"
                 :timeout-ms 3000
                 :expect-debugcon '("stage1: loading boot bench"
                                    "kernel: bench complete"))
    (test-source "examples/cpu/demo-memops.bwo"
                 :timeout-ms 5000
                 :expect-debugcon '("stage1: loading memory ops demo"
                                    "kernel: memory ops demo reached"
                                    "kernel: memory ops checks passed"))
    (test-source "examples/cpu/demo-sysinstr.bwo"
                 :timeout-ms 5000
                 :expect-debugcon '("stage1: loading sysinstr demo"
                                    "kernel: sysinstr demo reached"
                                    "kernel: sysinstr demo complete"))
    (test-source "examples/devices/demo-ata.bwo"
                 :timeout-ms 5000
                 :expect-debugcon '("stage1: loading ata demo"
                                    "kernel: ata demo reached"
                                    "kernel: ata read ok (sig=0xAA55)"))
    (test-source "examples/devices/demo-memmap.bwo"
                 :timeout-ms 5000
                 :expect-debugcon '("stage1: loading memmap demo"
                                    "kernel: memmap demo reached"
                                    "kernel: e820 ok"))
    (test-source "examples/devices/demo-physalloc.bwo"
                 :timeout-ms 5000
                 :expect-debugcon '("stage1: loading physalloc demo"
                                    "kernel: physalloc demo reached"
                                    "kernel: phys-allocator ok"))
    (test-source "examples/exec/demo-context.bwo"
                 :timeout-ms 5000
                 :expect-debugcon '("stage1: loading context demo"
                                    "kernel: context demo reached"
                                    "kernel: step B (running in thread-b)"
                                    "kernel: step C (resumed in thread-a)"
                                    "kernel: context-switch ok"))
    (test-source "examples/exec/demo-syscalls.bwo"
                 :timeout-ms 5000
                 :expect-debugcon '("stage1: loading syscalls demo"
                                    "kernel: syscalls demo reached"
                                    "syscall: vec 0"
                                    "syscall: vec 1"
                                    "syscall: vec 2"
                                    "kernel: syscall table ok"))
    (test-source "examples/exec/demo-userspace.bwo"
                 :timeout-ms 5000
                 :expect-debugcon '("stage1: loading userspace demo"
                                    "kernel: userspace demo reached"
                                    "kernel: dropping to ring 3"
                                    "kernel: int 0x80 from ring 3 received"
                                    "kernel: userspace demo passed"))
    (test-source "examples/exec/demo-sysenter.bwo"
                 :timeout-ms 5000
                 :expect-debugcon '("stage1: loading sysenter demo"
                                    "kernel: sysenter demo reached"
                                    "kernel: sysenter MSRs programmed"
                                    "kernel: sysenter handler ran"
                                    "kernel: sysenter demo passed"))
    (test-source "examples/devices/demo-storage.bwo"
                 :timeout-ms 5000
                 :expect-debugcon '("kernel: storage demo reached"
                                    "kernel: partition-table-read ok"
                                    "kernel: partition-find type 0x83 ok"
                                    "kernel: volume bound to partition 0"
                                    "kernel: volume-read sentinel match"
                                    "kernel: storage demo passed"))
    (format t "Bootwright QEMU tests passed.~%")))
