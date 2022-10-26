(defsystem "combinator"
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
  :in-order-to ((test-op (test-op "combinator/tests"))))

(defsystem "combinator/tests"
  :author ""
  :license ""
  :depends-on ("combinator"
               "fiveam")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for combinator"
  :perform (test-op (op s) (symbol-call :combinator/tests/main '#:test-main-suite)))
