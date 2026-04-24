(asdf:defsystem #:bootwright
  :description "Bootwright: a Common Lisp bare-metal OS framework with a built-in assembler."
  :author "Bruno"
  :license "MIT"
  :serial t
  :components ((:file "src/package")
               (:file "src/util")
               (:file "src/assembler")
               (:file "src/model")
               (:file "src/dsl")
               (:file "src/image")
               (:file "src/source")))
