(require "asdf")

(let* ((root (make-pathname :name nil :type nil :defaults (or *load-truename*
                                                              *compile-file-truename*)))
       (asd (merge-pathnames #P"bootwright.asd" root)))
  (asdf:load-asd asd)
  (asdf:operate 'asdf:load-source-op :bootwright)
  (let ((builder (symbol-function (find-symbol "BUILD-OS-SOURCE-FILE" "BOOTWRIGHT"))))
    (dolist (spec '(("examples/demo-real.bwo" "out/bootwright-demo.img")
                    ("examples/demo-protected.bwo" "out/bootwright-protected.img")
                    ("examples/demo-assets.bwo" "out/bootwright-assets.img")
                    ("examples/demo-timer.bwo" "out/bootwright-timer.img")))
      (destructuring-bind (source output) spec
        (let ((pathname (funcall builder
                                 (merge-pathnames source root)
                                 :output (merge-pathnames output root))))
          (format t "Bootwright image written to ~A~%" pathname))))))
