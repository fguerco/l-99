;;;; l-99.asd

(asdf:defsystem #:l-99
  :description "L-99 - 99 Lisp Problems"
  :author "Felipe Guerço Oliveira <felipeguerco@gmail.com>"
  :license  "MIT"
  :version "1.0.0"
  :serial t
  :depends-on (#:fg-utils #:parachute)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "lists")
                             (:file "arithmetic")))
               (:module "test"
                :serial t
                :components ((:file "lists"))))
  :perform (test-op (op system)
                    (uiop:symbol-call :parachute :test :l-99/tests)))
