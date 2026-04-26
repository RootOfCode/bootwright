(require "asdf")

(let* ((root (make-pathname :name nil :type nil :defaults (or *load-truename*
                                                              *compile-file-truename*)))
       (asd (merge-pathnames #P"bootwright.asd" root)))
  (asdf:load-asd asd)
  (asdf:operate 'asdf:load-source-op :bootwright)
  (let ((builder (symbol-function (find-symbol "BUILD-OS-SOURCE-FILE" "BOOTWRIGHT"))))
    (dolist (spec '(("examples/basics/demo-real.bwo"        "out/bootwright-demo.img")
                    ("examples/basics/demo-assets.bwo"      "out/bootwright-assets.img")
                    ("examples/protected/demo-protected.bwo" "out/bootwright-protected.img")
                    ("examples/devices/demo-timer.bwo"      "out/bootwright-timer.img")
                    ("examples/devices/demo-probe.bwo"      "out/bootwright-probe.img")
                    ("examples/devices/demo-keyboard.bwo"   "out/bootwright-keyboard.img")
                    ("examples/devices/demo-ata.bwo"        "out/bootwright-ata.img")
                    ("examples/cpu/demo-cpuinfo.bwo"        "out/bootwright-cpuinfo.img")
                    ("examples/cpu/demo-bench.bwo"          "out/bootwright-bench.img")
                    ("examples/cpu/demo-memops.bwo"         "out/bootwright-memops.img")
                    ("examples/cpu/demo-sysinstr.bwo"       "out/bootwright-sysinstr.img")))
      (destructuring-bind (source output) spec
        (let ((pathname (funcall builder
                                 (merge-pathnames source root)
                                 :output (merge-pathnames output root))))
          (format t "Bootwright image written to ~A~%" pathname))))))
