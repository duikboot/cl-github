(in-package :asdf-user)

(defsystem "cl-github"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("dexador"
               "alexandria"
               "jsown"
               "cl-json")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-github/tests"))))

(defsystem "cl-github/tests"
  :author ""
  :license ""
  :depends-on ("cl-github"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-github"
  :perform (test-op (op c) (symbol-call :rove :run c)))
