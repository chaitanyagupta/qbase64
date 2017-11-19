;;;; qbase64.asd

(asdf:defsystem #:qbase64
  :description "Describe qbase64 here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on ("trivial-gray-streams" "metabang-bind")
  :components ((:file "package")
               (:file "utils")
               (:file "qbase64")))

(asdf:defsystem "qbase64/test"
  :depends-on ("qbase64" "fiveam" "temporary-file")
  :perform (test-op (o s)
                    (uiop:symbol-call :fiveam '#:run!
                                      (list (uiop:find-symbol* '#:encoder '#:qbase64-test)
                                            (uiop:find-symbol* '#:decoder '#:qbase64-test))))
  :components ((:file "qbase64-test")))
