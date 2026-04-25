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
    (test-source "examples/demo-real.bwo"
                 :timeout-ms 3000
                 :expect-debugcon '("stage1: loading real-mode demo"
                                    "kernel: real-mode demo reached"))
    (test-source "examples/demo-protected.bwo"
                 :timeout-ms 5000
                 :expect-debugcon '("stage1: loading protected-mode demo"
                                    "kernel: protected mode + paging active"
                                    "kernel: returned from int 0x30"))
    (test-source "examples/demo-probe.bwo"
                 :timeout-ms 5000
                 :expect-serial '("[probe] COM1 online @ 38400 8N1"
                                  "[probe] PIT 20 Hz armed; idling."
                                  "[probe] +1 s"))
    (test-source "examples/demo-memops.bwo"
                 :timeout-ms 5000
                 :expect-debugcon '("stage1: loading memory ops demo"
                                    "kernel: memory ops demo reached"
                                    "kernel: memory ops checks passed"))
    (format t "Bootwright QEMU tests passed.~%")))
