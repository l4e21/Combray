(defsystem "combray"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:alexandria :serapeum)
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "init")
                 (:file "parser"))))
  :description ""
  :in-order-to ((test-op (test-op "combray/tests"))))

(defsystem "combray/tests"
  :author ""
  :license ""
  :depends-on ("combray"
               "fiveam")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for combray"
  :perform (test-op (op s) (symbol-call :combray/tests/main '#:test-main-suite)))
