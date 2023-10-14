;;;; l-99.asd

(asdf:defsystem #:l-99
  :description "L-99 - 99 Lisp Problems"
  :author "Felipe Guerço Oliveira <felipeguerco@gmail.com>"
  :license  "MIT"
  :version "1.0.0"
  :serial t
  :depends-on (#:alexandria #:fg-utils #:parachute)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "lists")))
               (:module "test"
                :serial t)))
