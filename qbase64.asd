;;;; qbase64.asd

(asdf:defsystem #:qbase64
  :description "Describe qbase64 here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (:trivial-gray-streams :metabang-bind)
  :components ((:file "package")
               (:file "utils")
               (:file "qbase64")))
