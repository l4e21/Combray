(defsystem #:combray
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:serapeum :tactile)
  :serial T
  :components ((:module "src"
                :components
                ((:file "models")
                 (:file "combinators"))))
  :description ""
  :in-order-to ((test-op (test-op "combray/tests"))))

(defsystem #:combray/tests
  :author ""
  :license ""
  :depends-on ("combray"
               "fiveam")
  :serial T
  :components ((:module "tests"
                :components
                ((:file "package")
                 (:file "main"))))
  :description "Test system for combray"
  :perform (test-op (op s) (symbol-call :combray/tests '#:debug-combray-suite)))
