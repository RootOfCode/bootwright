(require "asdf")

(let* ((root (make-pathname :name nil :type nil :defaults (or *load-truename*
                                                              *compile-file-truename*)))
       (asd (merge-pathnames #P"bootwright.asd" root)))
  (asdf:load-asd asd)
  (asdf:operate 'asdf:load-source-op :bootwright)
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
    (format t "Bootwright QEMU tests passed.~%")))
